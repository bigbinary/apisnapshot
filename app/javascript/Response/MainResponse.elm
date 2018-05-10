module Response.MainResponse exposing (..)

import Response.JsonViewer as JsonViewer
import Utils.HttpUtil as HttpUtil exposing (Response)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Html.Events exposing (..)
import RemoteData
import Date.Extra as Date


-- MODEL --


type ResponseViewing
    = Formatted
    | Raw


type alias Model =
    { response : RemoteData.WebData HttpUtil.Response
    , responseViewing : ResponseViewing
    , jsonViewer : JsonViewer.Model
    }


init : Model
init =
    { response = RemoteData.NotAsked
    , responseViewing = Formatted
    , jsonViewer = JsonViewer.init
    }



-- UPDATE --


type Msg
    = SetResponseViewType ResponseViewing
    | JsonViewerMsg JsonViewer.Msg
    | UpdateResponse (RemoteData.WebData HttpUtil.Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetResponseViewType v ->
            ( { model | responseViewing = v }, Cmd.none )

        JsonViewerMsg jMsg ->
            case model.response of
                RemoteData.Success response ->
                    let
                        ( updatedJv, subCmd ) =
                            JsonViewer.update jMsg model.jsonViewer
                    in
                        ( { model | jsonViewer = updatedJv }, Cmd.map JsonViewerMsg subCmd )

                _ ->
                    ( model, Cmd.none )

        UpdateResponse newResponse ->
            ( { model | response = newResponse }, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    let
        content =
            case model.response of
                RemoteData.NotAsked ->
                    text ""

                RemoteData.Loading ->
                    p [ class "Main__loading" ] [ text "Loading..." ]

                RemoteData.Failure error ->
                    errorMarkup error

                RemoteData.Success response ->
                    loadedMarkup response model
    in
        div [ class "row" ] [ div [ class "col" ] [ content ] ]


loadedMarkup : Response -> Model -> Html Msg
loadedMarkup response model =
    div []
        [ httpStatusMarkup response
        , bodyHeadersNavBar
        , div [ class "tab-content", id "nav-tabContent" ]
            [ headersTabMarkup response
            , bodyTabMarkup response model
            ]
        ]


headersTabMarkup : Response -> Html Msg
headersTabMarkup response =
    let
        headerRow ( key, value ) =
            tr [] [ td [] [ strong [] [ text key ] ], td [] [ text value ] ]
    in
        div [ class "tab-pane fade", id "response-headers" ]
            [ table [ class "table" ]
                (List.map
                    headerRow
                    (HttpUtil.decodeHeadersFromHitResponse response)
                )
            ]


bodyTabMarkup : Response -> Model -> Html Msg
bodyTabMarkup response model =
    div [ class "tab-pane fade show active", id "response-body" ]
        [ formattedOrRawView response model ]


formattedOrRawView : Response -> Model -> Html Msg
formattedOrRawView response model =
    case model.responseViewing of
        Raw ->
            rawResponseMarkup response

        Formatted ->
            formattedResponseMarkup response model


formattedResponseMarkup : Response -> Model -> Html Msg
formattedResponseMarkup response model =
    h5 []
        [ text "Formatted response"
        , a [ class "btn", href "javascript:void(0)", onClick (SetResponseViewType Raw) ] [ text "Switch to raw response" ]
        , pre [ class "api-res__res" ]
            [ span [ class "block" ] [ JsonViewer.view (JsonViewer.getRootNode response model.jsonViewer) |> Html.map JsonViewerMsg ] ]
        ]


errorMarkup : Http.Error -> Html Msg
errorMarkup httpError =
    case httpError of
        Http.BadUrl url ->
            p [ class "Error" ] [ text ("Bad Url! " ++ url) ]

        Http.Timeout ->
            p [ class "Error" ] [ text "Sorry the request timed out" ]

        Http.NetworkError ->
            p [ class "Error" ] [ text "There was a network error." ]

        Http.BadStatus response ->
            div [] [ p [ class "Error" ] [ text "Server returned an error." ], httpErrorMarkup response ]

        Http.BadPayload message response ->
            div [] [ p [ class "Error" ] [ text ("Bad payload error: " ++ message) ], httpErrorMarkup response ]


httpErrorMarkup : Http.Response String -> Html Msg
httpErrorMarkup response =
    div [ class "" ]
        [ httpStatusMarkup response
        , bodyHeadersNavBar
        , rawResponseMarkup response
        ]


httpStatusMarkup : Http.Response String -> Html msg
httpStatusMarkup response =
    let
        responseCreatedAtMarkup =
            case HttpUtil.decodeCreatedAtFromResponse response of
                Just dateString ->
                    p []
                        [ span
                            [ class "api-res-form__label" ]
                            [ strong [] [ text "Date: " ]
                            , formatAndLocalizeDatetime dateString |> text
                            ]
                        ]

                Nothing ->
                    Html.text ""
    in
        div [ class "api-res-form__response" ]
            [ h3 [] [ text "Response" ]
            , p []
                [ span
                    [ class "api-res-form__label" ]
                    [ strong [] [ text "Status: " ]
                    , HttpUtil.decodeStatusCodeFromResponse response |> toString |> text
                    ]
                ]
            , responseCreatedAtMarkup
            ]


formatAndLocalizeDatetime : String -> String
formatAndLocalizeDatetime dateString =
    case Date.fromIsoString dateString of
        Just date ->
            Date.toFormattedString "ddd MMMM y, h:mm a" date

        Nothing ->
            dateString


bodyHeadersNavBar : Html msg
bodyHeadersNavBar =
    nav [ class "nav nav-tabs api-res__req-tabs", id "body-headers", attribute "role" "bodyheaderslist" ]
        [ a
            [ class "nav-item nav-link active"
            , id "response-body-tab"
            , attribute "data-toggle" "tab"
            , href "#response-body"
            , attribute "role" "tab"
            ]
            [ text "Body" ]
        , a
            [ class "nav-item nav-link"
            , id "response-headers-tab"
            , attribute "data-toggle" "tab"
            , href "#response-headers"
            , attribute "role" "tab"
            ]
            [ text "Headers" ]
        ]


rawResponseMarkup : Http.Response String -> Html Msg
rawResponseMarkup response =
    let
        parsedResponse =
            HttpUtil.decodeResponseBodyToString response
    in
        div []
            [ h5 []
                [ text "Raw Response"
                , a [ class "btn", href "javascript:void(0)", onClick (SetResponseViewType Formatted) ] [ text "Switch to formatted response" ]
                ]
            , pre [ class "form-control" ] [ text parsedResponse ]
            ]
