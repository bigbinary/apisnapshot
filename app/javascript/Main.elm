module Main exposing (..)

import Navigation exposing (Location)
import Router exposing (..)
import Response.MainResponse
import Request.MainRequest
import Request.RequestParameters
import Request.RequestHeaders
import Views.NotFound
import Html exposing (Html, div, ul, li, a, text)
import RemoteData
import Http
import Utils.HttpUtil as HttpUtil exposing (..)
import Utils.HttpMethods as HttpMethods exposing (HttpMethod)
import Tuple exposing (..)


-- MODEL --


type alias RequestBasicAuthentication =
    { username : String
    , password : String
    }


type alias Model =
    { request : Request.MainRequest.Model
    , response : Response.MainResponse.Model
    , route : Route
    }


init : Location -> ( Model, Cmd Msg )
init location =
    updateRoute (Router.parseLocation location)
        { request = Request.MainRequest.init location
        , response = Response.MainResponse.init
        , route = HomeRoute
        }



-- VIEW --


view : Model -> Html Msg
view model =
    let
        homeRouteMarkup =
            div []
                [ Request.MainRequest.view model.request |> Html.map RequestMsg
                , Response.MainResponse.view model.response |> Html.map ResponseMsg
                ]
    in
        case model.route of
            Router.HomeRoute ->
                homeRouteMarkup

            Router.HitRoute _ ->
                homeRouteMarkup

            _ ->
                Views.NotFound.view model



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE --


type Msg
    = RequestMsg Request.MainRequest.Msg
    | ResponseMsg Response.MainResponse.Msg
    | OnSubmitResponse (RemoteData.WebData Response)
    | OnLocationChange Location
    | OnHitFetchResponse (RemoteData.WebData Response)


navigateToHitPermalinkCommand : Maybe String -> Cmd Msg
navigateToHitPermalinkCommand maybeToken =
    case maybeToken of
        Just token ->
            Navigation.newUrl ("#/hits/" ++ token)

        Nothing ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestMsg phrMsg ->
            let
                areRequestParametersValid =
                    Request.RequestParameters.valid (Request.MainRequest.getRequestParameters model.request)

                areRequestHeadersValid =
                    Request.RequestHeaders.valid (Request.MainRequest.getRequestHeaders model.request)

                shouldSubmit =
                    (Request.MainRequest.isUrlValid (Request.MainRequest.getRequestUrl model.request)) && areRequestParametersValid && areRequestHeadersValid

                ( updatedRequest, updatedResponse, subCmd ) =
                    case phrMsg of
                        Request.MainRequest.Submit ->
                            let
                                ( uReq, _ ) =
                                    Request.MainRequest.update (Request.MainRequest.SetError (not shouldSubmit)) model.request

                                ( uRes, subCmd ) =
                                    case shouldSubmit of
                                        True ->
                                            ( first <|
                                                (Response.MainResponse.update (Response.MainResponse.UpdateResponse RemoteData.Loading) model.response)
                                            , requestCommand model
                                            )

                                        _ ->
                                            ( model.response, Cmd.none )
                            in
                                ( uReq, uRes, subCmd )

                        _ ->
                            let
                                ( uReq, _ ) =
                                    Request.MainRequest.update phrMsg model.request
                            in
                                ( uReq, model.response, Cmd.none )
            in
                ( { model | request = updatedRequest, response = updatedResponse }, subCmd )

        ResponseMsg phrMsg ->
            let
                ( updatedResponse, subCmd ) =
                    Response.MainResponse.update phrMsg model.response
            in
                ( { model | response = updatedResponse }, Cmd.map ResponseMsg subCmd )

        OnLocationChange location ->
            updateRoute (parseLocation location) model

        OnHitFetchResponse response ->
            let
                ( updatedRequest, _ ) =
                    Request.MainRequest.update (Request.MainRequest.OnHitFetchResponse response) model.request

                ( updatedResponse, _ ) =
                    Response.MainResponse.update (Response.MainResponse.UpdateResponse response) model.response

                updatedModel =
                    case response of
                        RemoteData.Success successResponse ->
                            { model
                                | request = updatedRequest
                                , response = updatedResponse
                            }

                        _ ->
                            model
            in
                updatedModel ! []

        OnSubmitResponse response ->
            let
                cmd =
                    case response of
                        RemoteData.Success successResponse ->
                            HttpUtil.decodeTokenFromResponse successResponse
                                |> navigateToHitPermalinkCommand

                        _ ->
                            Cmd.none

                ( updatedResponse, _ ) =
                    Response.MainResponse.update (Response.MainResponse.UpdateResponse response) model.response
            in
                { model | response = updatedResponse } ! [ cmd ]


updateRoute : Route -> Model -> ( Model, Cmd Msg )
updateRoute route model =
    let
        cmd =
            case route of
                HitRoute token ->
                    fetchHitDataCommand token

                _ ->
                    Cmd.none

        ( response, request ) =
            if cmd == Cmd.none then
                ( first <|
                    Response.MainResponse.update (Response.MainResponse.UpdateResponse RemoteData.NotAsked) model.response
                , first <|
                    (Request.MainRequest.update Request.MainRequest.EmptyRequest model.request)
                )
            else
                ( first <|
                    Response.MainResponse.update (Response.MainResponse.UpdateResponse RemoteData.Loading) model.response
                , model.request
                )
    in
        { model | route = route, request = request, response = response } ! [ cmd ]


requestCommand : Model -> Cmd Msg
requestCommand model =
    let
        requestPath =
            "/api_responses"

        requestBody =
            Http.jsonBody (Request.MainRequest.encodeRequest model.request)

        request =
            HttpUtil.buildRequest requestPath HttpMethods.Post requestBody
    in
        RemoteData.sendRequest request
            |> Cmd.map OnSubmitResponse


fetchHitDataCommand : String -> Cmd Msg
fetchHitDataCommand token =
    let
        requestPath =
            "/api_responses/" ++ token

        request =
            HttpUtil.buildRequest requestPath HttpMethods.Get Http.emptyBody
    in
        RemoteData.sendRequest request
            |> Cmd.map OnHitFetchResponse



-- MAIN --


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
