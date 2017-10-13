module Update exposing (update)

import JsonViewer
import LocalStorageData exposing (..)
import Models exposing (Model, PageState(..), firebaseConfigLocalStorageKey)
import Msgs exposing (Msg)
import Navigation
import Pages.Preferences exposing (stringifyFirebaseConfig)
import Ports exposing (..)
import RequestParameters exposing (..)
import Router exposing (parseLocation)
import Set
import HitEncoder
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


updateModelWithResponse : Model -> Http.Response String -> Model
updateModelWithResponse model httpResponse =
    { model
        | pageState =
            Loaded
                { raw = httpResponse
                , collapsedNodePaths = Set.empty
                , json =
                    JsonViewer.fromJSVal (HttpUtil.parseResponseBodyToJson httpResponse)
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

        Msgs.ResponseAvailable response ->
            let
                updatedModel =
                    case response of
                        Ok value ->
                            updateModelWithResponse model value

                        Err error ->
                            updateErrorResponse model error

                command =
                    updatedModel |> HitEncoder.encode |> Ports.firebaseSaveHit
            in
                updatedModel ! [ command ]

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

                _ ->
                    ( model, Cmd.none )

        Msgs.AddRequestParameter ->
            ( { model | requestParameters = pushBlank model.requestParameters }
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

        Msgs.HttpMethodsDropdownChange selectedHttpMethodString ->
            ( { model | httpMethod = parse selectedHttpMethodString }, Cmd.none )

        Msgs.OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )

        Msgs.OnLocalStorageSet response ->
            if response == "true" then
                { model | firebaseSdkInitializationState = Models.Initializing }
                    ! [ localStorageGet firebaseConfigLocalStorageKey ]
            else
                model ! []

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

                firebaseConfigString =
                    stringifyFirebaseConfig dirtFirebaseConfig
            in
                ( newModel, Ports.firebaseInitialize firebaseConfigString )

        Msgs.OnFirebaseInitialize response ->
            let
                newState =
                    if response.success then
                        Models.Intialized
                    else
                        Models.InitializationError response.error
            in
                { model | firebaseSdkInitializationState = newState } ! []

        Msgs.OnFirebaseSaveHit response ->
            let
                _ =
                    Debug.log "UUID of the hit saved at: " response
            in
                model ! []

        Msgs.PreferencesMsg subMsg ->
            let
                ( pageModel, cmd ) =
                    Pages.Preferences.update subMsg model.dirtyFirebaseConfig

                newModel =
                    { model | dirtyFirebaseConfig = pageModel }
            in
                ( newModel, Cmd.map Msgs.PreferencesMsg cmd )
