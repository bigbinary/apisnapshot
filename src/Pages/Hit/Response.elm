module Pages.Hit.Response exposing (..)

import Msgs exposing (Msg)
import JsonViewer
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Models exposing (Model)
import Html.Events exposing (..)


view : Model -> Html Msg
view model =
    case model.pageState of
        Models.Empty ->
            text ""

        Models.Loading ->
            p [ class "Main__loading" ] [ text "Loading..." ]

        Models.Error error ->
            errorMarkup error

        Models.Loaded response ->
            loadedMarkup response


loadedMarkup : Models.Response -> Html Msg
loadedMarkup response =
    div []
        [ httpStatusMarkup response.raw
        , bodyHeadersNavBar
        , div [ class "tab-content", id "nav-tabContent" ]
            [ headersTabMarkup response
            , bodyTabMarkup response
            ]
        ]


headersTabMarkup : Models.Response -> Html Msg
headersTabMarkup response =
    div [ class "tab-pane fade", id "response-headers" ]
        [ text (toString response.headers) ]


bodyTabMarkup : Models.Response -> Html Msg
bodyTabMarkup response =
    div [ class "tab-pane fade show active", id "response-body" ]
        [ formattedOrRawView response ]


formattedOrRawView : Models.Response -> Html Msg
formattedOrRawView response =
    if response.viewing == Models.Raw then
        rawResponseMarkup response.raw
    else
        formattedResponseMarkup response


formattedResponseMarkup : Models.Response -> Html Msg
formattedResponseMarkup response =
    let
        rootNode =
            { jsonVal = response.json
            , nodePath = JsonViewer.rootNodePath
            , depth = 0
            , collapsedNodePaths = response.collapsedNodePaths
            }
    in
        h5 []
            [ text "Formatted response"
            , a [ class "btn", href "#", onClick Msgs.ShowRawResponse ] [ text "switch to raw response" ]
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
    div [ class "api-res-form__response" ]
        [ h3 [] [ text "Response" ]
        , p [] [ span [ class "api-res-form__label" ] [ text ("Status: " ++ toString response.status.code) ] ]
        , p [] [ text response.status.message ]
        , p [] [ span [ class "api-res-form__label" ] [ text ("Date: display date here") ] ]
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
            , a [ class "btn", href "#", onClick Msgs.ShowFormattedResponse ] [ text "switch to formatted response" ]
            ]
        , textarea [ class "form-control" ] [ text response.body ]
        ]
