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
import Pages.Hit.RequestBody as RequestBody exposing (requestBodyEncoder)
import Models exposing (..)
import Util exposing (DropDownAction(..), dropDownActionToString)
import Util
            

encodeRequest : Model -> Json.Encode.Value
encodeRequest ({ request } as model) =
    let
        attributes =
            [ ( "url", string request.url )
            , ( "method", string <| HttpMethods.toString <| request.httpMethod )
            , ( "request_parameters", requestParametersEncoder request.requestParameters )
            ]
       
        addBody atr =
            case request.requestBody of
                Just rb ->
                    let
                        bodyHeader =
                            case rb.bodyType of
                                Util.BodyText -> "text/plain"
                                Util.BodyJSON -> "application/json"
                                    
                        currentHeaders = request.requestHeaders
                        contentTypeHeader = 
                            { key = "content_type"
                            , value = bodyHeader
                            }
                        newHeaders = Dict.insert (Dict.size currentHeaders) contentTypeHeader currentHeaders
                    in
                        atr ++ [ ("request_body", string rb.value)] ++ [( "request_headers", requestHeadersEncoder newHeaders)]
            
                Nothing ->
                    atr ++ [( "request_headers", requestHeadersEncoder request.requestHeaders )]
    in
        Json.Encode.object (addBody attributes)


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
        , requestParametersView request model.showErrors
        , requestHeadersView request model.showErrors
        , requestBodyView request model.showErrors
        ]


isUrlValid : String -> Bool
isUrlValid =
    Util.isStringPresent


urlInputField : Model -> Html Msg
urlInputField model =
    let
        defaultClass =
            "input form-control required"

        shouldShowError =
            model.showErrors && not (isUrlValid model.request.url)

        updatedClass =
            if shouldShowError then
                defaultClass ++ " is-invalid"
            else
                defaultClass

        viewValidationError =
            if shouldShowError then
                div [ class "invalid-feedback" ] [ text "Please enter a URL" ]
            else
                text ""
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
                , onClick <| Msgs.MoreActionsDropdownChange DDAddParameter
                ]
                [ text <| dropDownActionToString DDAddParameter ]
            , a
                [ class "dropdown-item"
                , href "javascript:void(0)"
                , onClick <| Msgs.MoreActionsDropdownChange DDAddHeader
                ]
                [ text <| dropDownActionToString DDAddHeader ]
            , a
                [ class "dropdown-item"
                , href "javascript:void(0)"
                , onClick <| Msgs.MoreActionsDropdownChange DDAddBody
                ]
                [ text <| dropDownActionToString DDAddBody]
            ]
        ]


requestParametersView : Request -> Bool -> Html Msg
requestParametersView { requestParameters } showErrors =
    if Dict.isEmpty requestParameters then
        text ""
    else
        RequestParameters.view requestParameters showErrors


requestHeadersView : Request -> Bool -> Html Msg
requestHeadersView { requestHeaders } showErrors =
    if Dict.isEmpty requestHeaders then
        text ""
    else
        RequestHeaders.view requestHeaders showErrors

requestBodyView : Request -> Bool -> Html Msg
requestBodyView { requestBody } showErrors =
    case requestBody of
        Nothing ->
            text ""
        Just b ->
            RequestBody.view b.value b.bodyType showErrors


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
