module Pages.Hit.Response exposing (..)

import Msgs exposing (Msg)
import JsonViewer
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Models


view model =
    case model.pageState of
        Models.Empty ->
            text ""

        Models.Loading ->
            p [ class "Main__loading" ] [ text "Loading..." ]

        Models.Error error ->
            errorMarkup error

        Models.Loaded response ->
            responseMarkup response


responseMarkup : Models.Response -> Html Msg
responseMarkup response =
    div []
        [ httpStatusMarkup response.raw
        , div [ class "tab-content", id "nav-tabContent" ]
            [ headersMarkup response
            , bodyMarkup response
            ]
        ]


headersMarkup response =
    div [ class "tab-pane fade", id "response-headers" ]
        [ text "this is headers" ]


bodyMarkup response =
    let
        rootNode =
            { jsonVal = response.json
            , nodePath = JsonViewer.rootNodePath
            , depth = 0
            , collapsedNodePaths = response.collapsedNodePaths
            }
    in
        div [ class "tab-pane fade show active", id "response-body" ]
            [ a [ class "btn" ] [ text "View raw" ]
            , pre [ class "api-res__res" ]
                [ span [ class "block" ]
                    [ JsonViewer.view rootNode
                    ]
                ]
            , httpRawResponseMarkup response.raw
            ]


errorMarkup : Http.Error -> Html msg
errorMarkup error =
    case error of
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


httpErrorMarkup : Http.Response String -> Html msg
httpErrorMarkup response =
    div [ class "" ]
        [ httpStatusMarkup response, httpRawResponseMarkup response ]


httpStatusMarkup : Http.Response String -> Html msg
httpStatusMarkup response =
    div [ class "api-res-form__response" ]
        [ h3 [] [ text "Response" ]
        , p [] [ span [ class "api-res-form__label" ] [ text ("Status: " ++ toString response.status.code) ] ]
        , p [] [ text response.status.message ]
        , p [] [ span [ class "api-res-form__label" ] [ text ("Date: display date here") ] ]
        , bodyHeadersRow
        ]


bodyHeadersRow =
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


httpRawResponseMarkup : Http.Response String -> Html msg
httpRawResponseMarkup response =
    div []
        [ h3 [] [ text "Raw Response Body" ]
        , pre [] [ code [] [ text response.body ] ]
        ]
