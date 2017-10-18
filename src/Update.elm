module Update exposing (update)

import JsonViewer
import Models exposing (Model, PageState(..))
import Msgs exposing (Msg)
import Navigation
import Pages.Preferences
import Pages.Hit.RequestParameters exposing (..)
import Router exposing (parseLocation)
import Set
import HttpUtil
import Http
import HttpMethods exposing (parse)
import Dict


requestCommand : Model -> Cmd Msg
requestCommand model =
    let
        encodedUrl =
            HttpUtil.encodeUrl model.request.url model.request.requestParameters

        request =
            HttpUtil.buildRequest encodedUrl model.request.httpMethod
    in
        Http.send Msgs.ResponseAvailable request


updateModelWithResponse : Model -> Http.Response String -> Model
updateModelWithResponse model httpResponse =
    let
        response =
            { raw = httpResponse
            , collapsedNodePaths = Set.empty
            , json = JsonViewer.fromJSVal (HttpUtil.parseResponseBodyToJson httpResponse)
            , headers = parseRespondeHeadersToJson httpResponse
            , viewing = Models.Formatted
            }
    in
        { model | pageState = Loaded response }


parseRespondeHeadersToJson : Http.Response String -> List ( String, String )
parseRespondeHeadersToJson httpResponse =
    Dict.toList httpResponse.headers


updateErrorResponse : Model -> Http.Error -> Model
updateErrorResponse model httpError =
    { model | pageState = Error httpError }


changeUrl : Model -> String -> Model
changeUrl model newUrl =
    let
        currentRequest =
            model.request

        isUrlEmpty =
            newUrl |> String.trim |> String.isEmpty

        error =
            if isUrlEmpty then
                Just "Please enter a url"
            else
                Nothing

        newRequest =
            { currentRequest | url = newUrl, urlError = error }
    in
        { model | request = newRequest }


updateModelWithViewingStatus : Model -> Models.ResponseViewing -> Model
updateModelWithViewingStatus model viewing =
    case model.pageState of
        Models.Loaded response ->
            let
                newPageState =
                    Models.Loaded { response | viewing = viewing }
            in
                { model | pageState = newPageState }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.ChangeUrl newUrl ->
            ( changeUrl model newUrl
            , Cmd.none
            )

        Msgs.ShowRawResponse ->
            let
                newModel =
                    updateModelWithViewingStatus model Models.Raw
            in
                ( newModel, Cmd.none )

        Msgs.ShowFormattedResponse ->
            let
                newModel =
                    updateModelWithViewingStatus model Models.Formatted
            in
                ( newModel, Cmd.none )

        Msgs.Submit ->
            let
                validUrl =
                    case model.request.urlError of
                        Just _ ->
                            False

                        Nothing ->
                            True

                shouldSubmit =
                    validUrl && valid model.request.requestParameters
            in
                if shouldSubmit then
                    ( { model | pageState = Models.Loading }, requestCommand model )
                else
                    ( model, Cmd.none )

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

                _ ->
                    ( model, Cmd.none )

        Msgs.AddRequestParameter ->
            let
                newRequestParameters =
                    pushBlank model.request.requestParameters

                currentRequest =
                    model.request

                newRequest =
                    { currentRequest | requestParameters = newRequestParameters }
            in
                ( { model | request = newRequest }, Cmd.none )

        Msgs.ChangeRequestParameterName index newName ->
            let
                currentRequest =
                    model.request

                newRequestParameters =
                    updateName index newName model.request.requestParameters

                newRequest =
                    { currentRequest | requestParameters = newRequestParameters }
            in
                ( { model | request = newRequest }, Cmd.none )

        Msgs.ChangeRequestParameterValue index newValue ->
            let
                currentRequest =
                    model.request

                newRequestParameters =
                    updateValue index newValue model.request.requestParameters

                newRequest =
                    { currentRequest | requestParameters = newRequestParameters }
            in
                ( { model | request = newRequest }, Cmd.none )

        Msgs.DeleteRequestParameter index ->
            let
                currentRequest =
                    model.request

                newRequestParameters =
                    remove index model.request.requestParameters

                newRequest =
                    { currentRequest | requestParameters = newRequestParameters }
            in
                ( { model | request = newRequest }, Cmd.none )

        Msgs.HttpMethodsDropdownChange selectedHttpMethodString ->
            let
                currentRequest =
                    model.request

                newRequest =
                    { currentRequest | httpMethod = parse selectedHttpMethodString }
            in
                ( { model | request = newRequest }, Cmd.none )

        Msgs.OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )
