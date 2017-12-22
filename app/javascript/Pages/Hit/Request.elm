module Pages.Hit.Request exposing (..)

import Dict exposing (Dict)
import Json.Encode exposing (..)
import Json.Decode
import HttpMethods exposing (HttpMethod, avaialableHttpMethodsString)
import Msgs exposing (Msg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Hit.RequestParameters as RequestParameters
    exposing
        ( RequestParameter
        , RequestParameters
        , requestParametersEncoder
        )
import Pages.Hit.RequestHeaders as RequestHeaders
    exposing
        ( RequestHeader
        , RequestHeaders
        , requestHeadersEncoder
        )
import Models exposing (..)
import Util


encodeRequest : Model -> Json.Encode.Value
encodeRequest ({ request } as model) =
    let
        attributes =
            [ ( "url", string request.url )
            , ( "method", string <| HttpMethods.toString <| request.httpMethod )
            , ( "request_parameters", requestParametersEncoder request.requestParameters )
            , ( "request_headers", requestHeadersEncoder request.requestHeaders )
            ]
    in
        Json.Encode.object attributes


view : Model -> Html Msg
view model =
    div [ class "row form-controls" ] [ formView model ]


formView : Model -> Html Msg
formView ({ request } as model) =
    Html.form
        [ class "bootstrap-center-form api-req-form__form col"
        , onSubmit Msgs.Submit
        , action "javascript:void(0)"
        ]
        [ div [ class "api-req-form__url-group" ]
            [ httpMethodDropdown request.httpMethod
            , urlInputField model
            , morePullDownMenu
            , button [ class "btn btn-primary", type_ "Submit" ] [ text "SEND" ]
            ]
        , requestParametersView request
        , requestHeadersView request
        ]


isUrlValid : String -> Bool
isUrlValid =
    Util.isStringPresent


urlInputField : Model -> Html Msg
urlInputField model =
    let
        defaultClass =
            "input form-control required"

        updatedClass =
            if isUrlValid model.request.url then
                defaultClass
            else
                defaultClass ++ " is-invalid"

        viewValidationError =
            if isUrlValid model.request.url then
                text ""
            else
                div [ class "invalid-feedback" ]
                    [ text "Please enter a URL" ]
    in
        div [ class "api-req-form__url-control" ]
            [ input
                [ class updatedClass
                , name "url"
                , type_ "text"
                , placeholder "Enter URL here"
                , onInput Msgs.ChangeUrl
                , value model.request.url
                ]
                []
            , viewValidationError
            ]


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
                , href "javascript:void(0)"
                , onClick (Msgs.MoreActionsDropdownChange "Add Parameter")
                ]
                [ text "Add Parameter" ]
            , a
                [ class "dropdown-item"
                , href "javascript:void(0)"
                , onClick (Msgs.MoreActionsDropdownChange "Add Header")
                ]
                [ text "Add Header" ]
            ]
        ]


requestParametersView { requestParameters } =
    if Dict.isEmpty requestParameters then
        text ""
    else
        RequestParameters.view requestParameters


requestHeadersView { requestHeaders } =
    if Dict.isEmpty requestHeaders then
        text ""
    else
        RequestHeaders.view requestHeaders


httpMethodDropdown : HttpMethod -> Html Msg
httpMethodDropdown selectedHttpMethod =
    div []
        [ select
            [ class "form-control required"
            , on "change" <| Json.Decode.map Msgs.HttpMethodsDropdownChange targetValue
            ]
            (avaialableHttpMethodsString
                |> List.map (HttpMethods.toString selectedHttpMethod |> httpMethodDropdownOption)
            )
        ]


httpMethodDropdownOption : String -> String -> Html msg
httpMethodDropdownOption selectedMethodString httpMethodString =
    option
        [ value httpMethodString
        , selected (selectedMethodString == httpMethodString)
        ]
        [ text httpMethodString ]
