module Pages.Home exposing (..)

import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import HttpMethods exposing (HttpMethod, avaialableHttpMethodsString)
import Json.Decode
import JsonViewer
import Models exposing (..)
import Msgs exposing (Msg)
import RequestParameters


view : Model -> Html Msg
view model =
    let
        responseView =
            case model.pageState of
                Empty ->
                    text ""

                Loading ->
                    p [ class "Main__loading" ] [ text "Loading..." ]

                Error error ->
                    errorMarkup error

                Loaded response ->
                    responseMarkup response

        requestParametersView =
            if Array.isEmpty model.requestParameters then
                text ""
            else
                div []
                    [ div [ class "form-group__label" ]
                        [ span [] [ text "Request Parameters" ]
                        , a [ href "javascript:void(0)", class "devise-links", onClick Msgs.AddRequestParameter ]
                            [ text "Add Parameter" ]
                        ]
                    , RequestParameters.view model.requestParameters
                    ]
    in
        div [ class "row form-controls text-center" ]
            [ Html.form [ class "bootstrap-center-form api-req-form__form", onSubmit Msgs.Submit, action "javascript:void(0)" ]
                [ div [ class "api-req-form__url-group" ]
                    [ httpMethodDropdown model.httpMethod
                    , div [ class "api-req-form__url-control" ]
                        [ input
                            [ class "input form-control required"
                            , name "url"
                            , type_ "text"
                            , placeholder "Enter url here"
                            , onInput Msgs.ChangeUrl
                            , value model.url
                            ]
                            []
                        ]
                    , div [ class "api-req-form__btn-group btn-group" ]
                        [ select
                            [ class "UrlForm__moreActionsDropdown"
                            , value "More"
                            , on "change" (Json.Decode.map Msgs.MoreActionsDropdownChange targetValue)
                            ]
                            [ option [ value "More" ] [ text "More" ]
                            , option [ value "Add Parameter" ] [ text "Add Parameter" ]
                            ]
                        ]
                    , button [ class "btn btn-primary", type_ "Submit" ] [ text "SEND" ]
                    ]
                , div [ class "error" ] [ text (Maybe.withDefault "" model.error) ]
                , div [ class "form-group RequestParameters" ] [ requestParametersView ]
                , div [ class "Result" ] [ responseView ]
                ]
            ]


httpMethodDropdownOption : String -> Html msg
httpMethodDropdownOption httpMethodString =
    option [ value httpMethodString ] [ text httpMethodString ]


httpMethodDropdown : HttpMethod -> Html Msg
httpMethodDropdown selectedHttpMethod =
    div []
        [ select
            [ class "form-control required"
            , value <| HttpMethods.toString selectedHttpMethod
            , on "change" <| Json.Decode.map Msgs.HttpMethodsDropdownChange targetValue
            ]
            (List.map httpMethodDropdownOption avaialableHttpMethodsString)
        ]


httpStatusMarkup : Http.Response String -> Html msg
httpStatusMarkup response =
    div [ class "api-res-form__response" ]
        [ h3 [] [ text "Response" ]
        , p [] [ span [ class "api-res-form__label" ] [ text ("Status: " ++ toString response.status.code) ] ]
        , p [] [ text response.status.message ]
        , p [] [ span [ class "api-res-form__label" ] [ text ("Date: display date here") ] ]
        , ul [ class "nav nav-tabs api-res__req-tabs" ]
            [ li [ class "active" ] [ a [] [ text "Body" ] ]
            , li [ class "" ] [ a [] [ text "Headers" ] ]
            ]
        ]


httpRawResponseMarkup : Http.Response String -> Html msg
httpRawResponseMarkup response =
    div []
        [ h3 [] [ text "Raw Response Body" ]
        , pre [] [ code [] [ text response.body ] ]
        ]


httpErrorMarkup : Http.Response String -> Html msg
httpErrorMarkup response =
    div []
        [ httpStatusMarkup response, httpRawResponseMarkup response ]


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


responseMarkup : Models.Response -> Html Msg
responseMarkup response =
    let
        rootNode =
            { jsonVal = response.json
            , nodePath = JsonViewer.rootNodePath
            , depth = 0
            , collapsedNodePaths = response.collapsedNodePaths
            }
    in
        div []
            [ httpStatusMarkup response.raw
            , div [ class "Result__jsonView" ]
                [ a [ class "btn" ] [ text "View raw" ]
                , pre [ class "api-res__res" ]
                    [ span [ class "block" ]
                        [ JsonViewer.view rootNode
                        ]
                    ]
                ]
            , httpRawResponseMarkup response.raw
            ]
