module Pages.Hit.Request exposing (..)

import Array
import Json.Decode
import HttpMethods exposing (HttpMethod, avaialableHttpMethodsString)
import Msgs exposing (Msg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Hit.RequestParameters
import Models exposing (..)


view : Model -> Html Msg
view model =
    div [ class "row form-controls text-center" ] [ formView model ]


formView : Model -> Html Msg
formView model =
    Html.form
        [ class "bootstrap-center-form api-req-form__form"
        , onSubmit Msgs.Submit
        , action "javascript:void(0)"
        ]
        [ div [ class "api-req-form__url-group" ]
            [ httpMethodDropdown model.request.httpMethod
            , urlInputField model
            , morePullDownMenu
            , button [ class "btn btn-primary", type_ "Submit" ] [ text "SEND" ]
            ]
        , requestParametersView model
        ]


urlInputField : Model -> Html Msg
urlInputField model =
    div [ class "api-req-form__url-control" ]
        [ input
            [ class (classForUrlField model)
            , name "url"
            , type_ "text"
            , placeholder "Enter url here"
            , onInput Msgs.ChangeUrl
            , value model.request.url
            ]
            []
        , urlEmptyErrorMessage model
        ]


classForUrlField : Model -> String
classForUrlField model =
    let
        defaultClass =
            "input form-control required"
    in
        case model.request.urlError of
            Nothing ->
                defaultClass

            Just error ->
                defaultClass ++ " is-invalid"


urlEmptyErrorMessage : Model -> Html Msg
urlEmptyErrorMessage model =
    case model.request.urlError of
        Nothing ->
            span [] []

        Just error ->
            div [ class "invalid-feedback" ] [ text error ]


morePullDownMenu =
    div [ class "dropdown api-req-form__btn-group btn-group" ]
        [ button
            [ type_ "button"
            , class "btn btn-primary dropdown-toggle"
            , attribute "data-toggle" "dropdown"
            , attribute "aria-haspopup" "true"
            , attribute "aria-expanded" "false"
            ]
            [ text "More" ]
        , div
            [ class "dropdown-menu", attribute "aria-labelledby" "dropdownMenuButton" ]
            [ a
                [ class "dropdown-item"
                , href "#"
                , onClick (Msgs.MoreActionsDropdownChange "Add Parameter")
                ]
                [ text "Add Parameter" ]
            ]
        ]


requestParametersView model =
    if Array.isEmpty model.request.requestParameters then
        text ""
    else
        Pages.Hit.RequestParameters.view model.request.requestParameters


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
