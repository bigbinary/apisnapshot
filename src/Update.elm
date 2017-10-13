module Update exposing (update)

import Array
import Http
import HttpMethods exposing (HttpMethod, parse, toString)
import JsonViewer
import LocalStorageData exposing (..)
import Models exposing (Model, PageState(..), firebaseConfigLocalStorageKey)
import Msgs exposing (Msg)
import Navigation
import Pages.Preferences
import Ports exposing (..)
import RequestParameters exposing (..)
import Assertions
import Router exposing (parseLocation)
import Set
import HttpUtil
import Http
import HttpMethods exposing (parse)


requestCommand : Model -> Cmd Msg
requestCommand model =
    let
        encodedUrl =
            HttpUtil.encodeUrl model.url model.requestParameters

        request =
            HttpUtil.buildRequest encodedUrl model.httpMethod
    in
        Http.send Msgs.ResponseAvailable request


verifyAssertion : Http.Response String -> Assertions.Assertion -> Assertions.Assertion
verifyAssertion httpResponse assertion =
    let
        isResponseStatusCode200 =
            Basics.toString (httpResponse.status.code) == assertion.value

        isAssertionTrue =
            assertion.key == "equals" && assertion.value == "200" && isResponseStatusCode200

        state =
            if isAssertionTrue then
                Assertions.PASSED
            else
                Assertions.FAILED
    in
        { assertion | state = state }


verifyAssertions : Model -> Http.Response String -> Model
verifyAssertions model httpResponse =
    let
        newAssertions =
            Array.map (verifyAssertion httpResponse) model.assertions
    in
        { model | assertions = newAssertions }


updateModelWithResponse : Model -> Http.Response String -> Model
updateModelWithResponse model httpResponse =
    let
        json =
            JsonViewer.fromJSVal (HttpUtil.parseResponseBodyToJson httpResponse)

        newModel =
            verifyAssertions model httpResponse
    in
        { newModel
            | pageState =
                Loaded
                    { raw = httpResponse
                    , collapsedNodePaths = Set.empty
                    , json = json
                    }
        }


updateErrorResponse : Model -> Http.Error -> Model
updateErrorResponse model error =
    { model | pageState = Error error }


changeUrl : Model -> String -> Model
changeUrl model newUrl =
    let
        isUrlEmpty =
            newUrl |> String.trim |> String.isEmpty

        error =
            if isUrlEmpty then
                Just "Please enter a url"
            else
                Nothing
    in
        { model | url = newUrl, error = error }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.ChangeUrl newUrl ->
            ( changeUrl model newUrl
            , Cmd.none
            )

        Msgs.Submit ->
            case model.error of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    ( { model | pageState = Models.Loading }, requestCommand model )

        Msgs.ResponseAvailable (Ok value) ->
            ( updateModelWithResponse model value, Cmd.none )

        Msgs.ResponseAvailable (Err error) ->
            ( updateErrorResponse model error, Cmd.none )

        Msgs.ToggleJsonCollectionView id ->
            ( case model.pageState of
                Loaded response ->
                    let
                        collapsedNodePaths =
                            response.collapsedNodePaths
                    in
                        if Set.member id collapsedNodePaths then
                            { model | pageState = Loaded { response | collapsedNodePaths = Set.remove id collapsedNodePaths } }
                        else
                            { model | pageState = Loaded { response | collapsedNodePaths = Set.insert id collapsedNodePaths } }

                _ ->
                    model
            , Cmd.none
            )

        Msgs.MoreActionsDropdownChange selectedOption ->
            case selectedOption of
                "Add Parameter" ->
                    update Msgs.AddRequestParameter model

                "Add Assertion" ->
                    update Msgs.AddAssertion model

                _ ->
                    let
                        _ =
                            Debug.log ("_" ++ selectedOption)
                    in
                        ( model, Cmd.none )

        Msgs.AddRequestParameter ->
            ( { model | requestParameters = pushBlank model.requestParameters }
            , Cmd.none
            )

        Msgs.AddAssertion ->
            ( { model | assertions = Assertions.pushBlank model.assertions }
            , Cmd.none
            )

        Msgs.ChangeRequestParameterName index newName ->
            ( { model | requestParameters = updateName index newName model.requestParameters }
            , Cmd.none
            )

        Msgs.ChangeRequestParameterValue index newValue ->
            ( { model | requestParameters = updateValue index newValue model.requestParameters }
            , Cmd.none
            )

        Msgs.DeleteRequestParameter index ->
            ( { model | requestParameters = remove index model.requestParameters }, Cmd.none )

        Msgs.ChangeAssertionName index newName ->
            ( { model | assertions = Assertions.updateName index newName model.assertions }
            , Cmd.none
            )

        Msgs.ChangeAssertionValue index newValue ->
            ( { model | assertions = Assertions.updateValue index newValue model.assertions }
            , Cmd.none
            )

        Msgs.DeleteAssertion index ->
            ( { model | assertions = Assertions.remove index model.assertions }, Cmd.none )

        Msgs.HttpMethodsDropdownChange selectedHttpMethodString ->
            ( { model | httpMethod = parse selectedHttpMethodString }, Cmd.none )

        Msgs.OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )

        Msgs.OnLocalStorageSet response ->
            let
                cmd =
                    Cmd.batch
                        [ localStorageGet firebaseConfigLocalStorageKey
                        , Navigation.newUrl "#"
                        ]
            in
                if response == "true" then
                    ( model, cmd )
                else
                    ( model, Cmd.none )

        Msgs.OnLocalStorageGet response ->
            let
                decodedFirebaseConfig =
                    Pages.Preferences.decodeFirebaseConfig response

                dirtFirebaseConfig =
                    case decodedFirebaseConfig of
                        LocalStorageData.Loading ->
                            model.dirtyFirebaseConfig

                        LocalStorageData.Success value ->
                            value

                        LocalStorageData.Failure error ->
                            model.dirtyFirebaseConfig

                newModel =
                    { model
                        | firebaseConfig = decodedFirebaseConfig
                        , dirtyFirebaseConfig = dirtFirebaseConfig
                    }
            in
                ( newModel, Cmd.none )

        Msgs.PreferencesMsg subMsg ->
            let
                ( pageModel, cmd ) =
                    Pages.Preferences.update subMsg model.dirtyFirebaseConfig

                newModel =
                    { model | dirtyFirebaseConfig = pageModel }
            in
                ( newModel, Cmd.map Msgs.PreferencesMsg cmd )
