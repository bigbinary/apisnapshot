module Pages.Hit.Response exposing (..)

import Msgs exposing (Msg)
import JsonViewer
import HttpUtil
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Models exposing (Model)
import Html.Events exposing (..)
import Response exposing (..)
import RemoteData


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
            [ table []
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
    if model.responseViewing == Raw then
        rawResponseMarkup response
    else
        formattedResponseMarkup response model


formattedResponseMarkup : Response -> Model -> Html Msg
formattedResponseMarkup response model =
    let
        rootNode =
            { jsonVal =
                JsonViewer.fromJSVal
                    (HttpUtil.decodeHitResponseBodyIntoJson response)
            , nodePath = JsonViewer.rootNodePath
            , depth = 0
            , collapsedNodePaths = model.collapsedNodePaths
            }
    in
        h5 []
            [ text "Formatted response"
            , a [ class "btn", href "javascript:void(0)", onClick Msgs.ShowRawResponse ] [ text "Switch to raw response" ]
            , pre [ class "api-res__res" ]
                [ span [ class "block" ] [ JsonViewer.view rootNode ] ]
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
                Just date ->
                    p []
                        [ span
                            [ class "api-res-form__label" ]
                            [ strong [] [ text "Date: " ], text date ]
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
    div []
        [ h5 []
            [ text "Raw Response"
            , a [ class "btn", href "javascript:void(0)", onClick Msgs.ShowFormattedResponse ] [ text "Switch to formatted response" ]
            ]
        , pre [ class "form-control" ] [ text response.body ]
        ]
