module Pages.Hit.Request exposing (..)

import Array
import Json.Decode
import HttpMethods exposing (HttpMethod, avaialableHttpMethodsString)
import Msgs exposing (Msg)
import JsonViewer
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Pages.Hit.RequestParameters
import Models exposing (..)


view : Model -> Html Msg
view model =
    div [ class "row form-controls text-center" ]
        [ form model (requestParametersView model)
        ]


urlInputField model =
    div [ class "api-req-form__url-control" ]
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


morePullDownMenu =
    div [ class "api-req-form__btn-group btn-group" ]
        [ select
            [ class "UrlForm__moreActionsDropdown"
            , value "More"
            , on "change" (Json.Decode.map Msgs.MoreActionsDropdownChange targetValue)
            ]
            [ option [ value "More" ] [ text "More" ]
            , option [ value "Add Parameter" ] [ text "Add Parameter" ]
            ]
        ]


errorMessage model =
    div [ class "error" ] [ text (Maybe.withDefault "" model.error) ]


form model requestParametersView =
    Html.form [ class "bootstrap-center-form api-req-form__form", onSubmit Msgs.Submit, action "javascript:void(0)" ]
        [ div [ class "api-req-form__url-group" ]
            [ httpMethodDropdown model.httpMethod
            , urlInputField model
            , morePullDownMenu
            , button [ class "btn btn-primary", type_ "Submit" ] [ text "SEND" ]
            ]
        , errorMessage model
        , div [ class "form-group" ] [ requestParametersView ]
        ]


requestParametersView model =
    if Array.isEmpty model.requestParameters then
        text ""
    else
        Pages.Hit.RequestParameters.view model.requestParameters


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


httpMethodDropdownOption : String -> Html msg
httpMethodDropdownOption httpMethodString =
    option [ value httpMethodString ] [ text httpMethodString ]
