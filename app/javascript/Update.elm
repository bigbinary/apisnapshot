module Update exposing (update, updateRoute)

import Models exposing (Model)
import Msgs exposing (Msg)
import Navigation
import Pages.Hit.Request
import Pages.Hit.RequestParameters exposing (..)
import Router exposing (parseLocation)
import Response exposing (..)
import Set
import HttpUtil
import Http
import HttpMethods exposing (parse)
import Dict
import Util exposing (isMaybeValuePresent, isStringEmpty)
import RemoteData exposing (WebData)


requestCommand : Model -> Cmd Msg
requestCommand model =
    let
        requestPath =
            "/api_responses"

        requestBody =
            Http.jsonBody (Pages.Hit.Request.encodeRequest model)

        request =
            HttpUtil.buildRequest requestPath HttpMethods.Post requestBody
    in
        RemoteData.sendRequest request
            |> Cmd.map Msgs.OnSubmitResponse


fetchHitDataCommand : String -> Cmd Msg
fetchHitDataCommand token =
    let
        requestPath =
            "/api_responses/" ++ token

        request =
            HttpUtil.buildRequest requestPath HttpMethods.Get Http.emptyBody
    in
        RemoteData.sendRequest request
            |> Cmd.map Msgs.OnHitFetchResponse


parseRespondeHeadersToJson : Http.Response String -> List ( String, String )
parseRespondeHeadersToJson httpResponse =
    Dict.toList httpResponse.headers


updateErrorResponse : Model -> Http.Error -> Model
updateErrorResponse model httpError =
    model


changeUrl : Model -> String -> Model
changeUrl model newUrl =
    let
        currentRequest =
            model.request

        error =
            if isStringEmpty newUrl then
                Just "Please enter a url"
            else
                Nothing

        newRequest =
            { currentRequest | url = newUrl, urlError = error }
    in
        { model | request = newRequest }


updateModelWithViewingStatus : Model -> ResponseViewing -> Model
updateModelWithViewingStatus model viewing =
    case model.response of
        RemoteData.Success response ->
            { model | responseViewing = viewing }

        _ ->
            model


navigateToHitPermalinkCommand : Maybe String -> Cmd Msg
navigateToHitPermalinkCommand maybeToken =
    case maybeToken of
        Just token ->
            Navigation.newUrl ("#/hits/" ++ token)

        Nothing ->
            Cmd.none


updateRoute : Models.Route -> Model -> ( Model, Cmd Msg )
updateRoute route model =
    let
        cmd =
            case route of
                Models.HitRoute token ->
                    fetchHitDataCommand token

                _ ->
                    Cmd.none

        response =
            if cmd == Cmd.none then
                RemoteData.NotAsked
            else
                RemoteData.Loading

        request =
            if cmd == Cmd.none then
                Models.emptyRequest
            else
                model.request
    in
        { model | route = route, request = request, response = response } ! [ cmd ]


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
                    updateModelWithViewingStatus model Raw
            in
                ( newModel, Cmd.none )

        Msgs.ShowFormattedResponse ->
            let
                newModel =
                    updateModelWithViewingStatus model Formatted
            in
                ( newModel, Cmd.none )

        Msgs.Submit ->
            let
                isUrlValid =
                    not (isMaybeValuePresent model.request.urlError)

                shouldSubmit =
                    isUrlValid && valid model.request.requestParameters
            in
                if shouldSubmit then
                    ( { model | response = RemoteData.Loading }, requestCommand model )
                else
                    ( model, Cmd.none )

        Msgs.ToggleJsonCollectionView id ->
            ( case model.response of
                RemoteData.Success response ->
                    let
                        collapsedNodePaths =
                            model.collapsedNodePaths
                    in
                        if Set.member id collapsedNodePaths then
                            { model | collapsedNodePaths = Set.remove id collapsedNodePaths }
                        else
                            { model | collapsedNodePaths = Set.insert id collapsedNodePaths }

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
            (parseLocation location |> updateRoute) model

        Msgs.OnSubmitResponse response ->
            let
                cmd =
                    case response of
                        RemoteData.Success successResponse ->
                            HttpUtil.decodeTokenFromResponse successResponse
                                |> navigateToHitPermalinkCommand

                        _ ->
                            Cmd.none
            in
                { model | response = response } ! [ cmd ]

        Msgs.OnHitFetchResponse response ->
            let
                updatedModel =
                    case response of
                        RemoteData.Success successResponse ->
                            { model
                                | request =
                                    HttpUtil.decodeHitResponseIntoRequest successResponse
                            }

                        _ ->
                            model
            in
                { updatedModel | response = response } ! []
