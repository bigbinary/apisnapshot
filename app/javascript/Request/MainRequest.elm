module Request.MainRequest exposing (..)

import Dict exposing (Dict)
import Json.Encode exposing (..)
import Json.Decode
import Utils.HttpMethods as HttpMethods exposing (HttpMethod, avaialableHttpMethodsString, parse)
import RemoteData
import Utils.Util as Util exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utils.HttpUtil as HttpUtil exposing (..)
import Request.RequestParameters as RequestParameters exposing (..)
import Request.RequestHeaders as RequestHeaders exposing (..)
import Request.RequestBody as RequestBody exposing (requestBodyEncoder, RequestBodyType, Msg, update)
import Request.RequestBasicAuthentication as RequestBasicAuthentication exposing (..)


-- MODEL --


type alias Model =
    { request : Request
    , showErrors : Bool
    }


init location =
    { request = emptyRequest
    , showErrors = False
    }



-- UPDATE --


type Msg
    = Submit
    | SetError Bool
    | ChangeUrl String
    | MoreActionsDropdownChange DropDownAction
    | HttpMethodsDropdownChange String
    | RequestHeaderMsg RequestHeaders.Msg
    | RequestParameterMsg RequestParameters.Msg
    | RequestBodyMsg RequestBody.Msg
    | RequestBasicAuthenticationMsg RequestBasicAuthentication.Msg
    | OnHitFetchResponse (RemoteData.WebData HttpUtil.Response)
    | EmptyRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetError err ->
            ( { model | showErrors = err }, Cmd.none )

        ChangeUrl newUrl ->
            ( changeUrl model newUrl, Cmd.none )

        MoreActionsDropdownChange selectedOption ->
            case selectedOption of
                DDAddParameter ->
                    let
                        currentRequest =
                            model.request

                        ( newParameters, subCmd ) =
                            RequestParameters.update RequestParameters.AddRequestParameter currentRequest.requestParameters

                        newRequest =
                            { currentRequest | requestParameters = newParameters }
                    in
                        ( { model | request = newRequest }, Cmd.map RequestParameterMsg subCmd )

                DDAddHeader ->
                    let
                        currentRequest =
                            model.request

                        ( newHeaders, subCmd ) =
                            RequestHeaders.update RequestHeaders.AddRequestHeader currentRequest.requestHeaders

                        newRequest =
                            { currentRequest | requestHeaders = newHeaders }
                    in
                        ( { model | request = newRequest }, Cmd.map RequestHeaderMsg subCmd )

                DDAddBody ->
                    let
                        currentRequest =
                            model.request

                        newRequest =
                            { currentRequest | requestBody = Just RequestBody.emptyBody }
                    in
                        ( { model | request = newRequest }, Cmd.none )

                DDAddBasicAuthentication ->
                    let
                        currentRequest =
                            model.request

                        newRequest =
                            { currentRequest | basicAuthentication = Just RequestBasicAuthentication.empty }
                    in
                        ( { model | request = newRequest }, Cmd.none )

        HttpMethodsDropdownChange selectedHttpMethodString ->
            let
                currentRequest =
                    model.request

                newRequest =
                    { currentRequest | httpMethod = parse selectedHttpMethodString }
            in
                ( { model | request = newRequest }, Cmd.none )

        RequestParameterMsg rpsMsg ->
            let
                currentRequest =
                    model.request

                ( newParameters, subCmd ) =
                    RequestParameters.update rpsMsg currentRequest.requestParameters

                newRequest =
                    { currentRequest | requestParameters = newParameters }
            in
                ( { model | request = newRequest }, Cmd.map RequestParameterMsg subCmd )

        RequestHeaderMsg rhsMsg ->
            let
                currentRequest =
                    model.request

                ( newHeaders, subCmd ) =
                    RequestHeaders.update rhsMsg currentRequest.requestHeaders

                newRequest =
                    { currentRequest | requestHeaders = newHeaders }
            in
                ( { model | request = newRequest }, Cmd.map RequestHeaderMsg subCmd )

        RequestBodyMsg rbsMsg ->
            let
                currentRequest =
                    model.request

                ( newBody, subCmd ) =
                    RequestBody.update rbsMsg currentRequest.requestBody

                newRequest =
                    { currentRequest | requestBody = newBody }
            in
                ( { model | request = newRequest }, Cmd.map RequestBodyMsg subCmd )

        OnHitFetchResponse response ->
            let
                updatedModel =
                    case response of
                        RemoteData.Success successResponse ->
                            { model
                                | request =
                                    HttpUtil.decodeHitResponseIntoRequest successResponse
                            }

                        _ ->
                            model
            in
                ( updatedModel, Cmd.none )

        EmptyRequest ->
            ( { model | request = emptyRequest }, Cmd.none )

        RequestBasicAuthenticationMsg rMsg ->
            case rMsg of
                RemoveBasicAuthentication ->
                    let
                        currentRequest =
                            model.request

                        newRequest =
                            { currentRequest | basicAuthentication = Nothing }
                    in
                        ( { model | request = newRequest }, Cmd.none )

                _ ->
                    let
                        currentRequest =
                            model.request

                        currentBa =
                            currentRequest.basicAuthentication

                        ( newRba, _ ) =
                            RequestBasicAuthentication.update rMsg currentBa

                        newRequest =
                            case currentBa of
                                Just b ->
                                    { currentRequest | basicAuthentication = newRba }

                                Nothing ->
                                    currentRequest
                    in
                        ( { model | request = newRequest }, Cmd.none )

        _ ->
            ( model, Cmd.none )


changeUrl : Model -> String -> Model
changeUrl model newUrl =
    let
        currentRequest =
            model.request
    in
        { model | showErrors = False, request = { currentRequest | url = newUrl } }


encodeRequest : Model -> Json.Encode.Value
encodeRequest ({ request } as model) =
    let
        attributes =
            [ ( "url", string request.url )
            , ( "method", string <| HttpMethods.toString <| request.httpMethod )
            , ( "request_parameters", requestParametersEncoder request.requestParameters )
            ]

        addBA atr =
            case request.basicAuthentication of
                Just ba ->
                    atr ++ [ ( "username", string ba.username ), ( "password", string ba.password ) ]

                Nothing ->
                    atr

        addBody atr =
            case request.requestBody of
                Just rb ->
                    let
                        bodyHeader =
                            case rb.bodyType of
                                RequestBody.BodyText ->
                                    "text/plain"

                                RequestBody.BodyJSON ->
                                    "application/json"

                        currentHeaders =
                            request.requestHeaders

                        contentTypeHeader =
                            { key = "content_type"
                            , value = bodyHeader
                            }

                        newHeaders =
                            Dict.insert (Dict.size currentHeaders) contentTypeHeader currentHeaders
                    in
                        atr ++ [ ( "request_body", string rb.value ) ] ++ [ ( "request_headers", requestHeadersEncoder newHeaders ) ]

                Nothing ->
                    atr ++ [ ( "request_headers", requestHeadersEncoder request.requestHeaders ) ]
    in
        Json.Encode.object (addBA <| addBody <| attributes)


getRequestUrl : Model -> String
getRequestUrl model =
    model.request.url


getRequestHeaders : Model -> RequestHeaders
getRequestHeaders model =
    model.request.requestHeaders


getRequestParameters : Model -> RequestParameters
getRequestParameters model =
    model.request.requestParameters



-- VIEW --


type DropDownAction
    = DDAddParameter
    | DDAddHeader
    | DDAddBody
    | DDAddBasicAuthentication


view : Model -> Html Msg
view model =
    div [ class "row form-controls" ] [ formView model ]


formView : Model -> Html Msg
formView ({ request } as model) =
    Html.form
        [ class "bootstrap-center-form api-req-form__form col"
        , onSubmit Submit
        , action "javascript:void(0)"
        ]
        [ div [ class "api-req-form__url-group" ]
            [ httpMethodDropdown request.httpMethod
            , urlInputField model
            , morePullDownMenu
            , button [ class "btn btn-primary", type_ "Submit" ] [ text "SEND" ]
            ]
        , requestBasicAuthenticationView request |> Html.map RequestBasicAuthenticationMsg
        , requestHeadersView request model.showErrors |> Html.map RequestHeaderMsg
        , requestParametersView request model.showErrors |> Html.map RequestParameterMsg
        , requestBodyView request model.showErrors |> Html.map RequestBodyMsg
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
                , onInput ChangeUrl
                , value model.request.url
                ]
                []
            , viewValidationError
            ]


morePullDownMenu =
    div [ class "dropdown api-req-form__btn-group btn-group" ]
        [ button
            [ type_ "button"
            , class "btn btn-default dropdown-toggle"
            , attribute "data-toggle" "dropdown"
            , attribute "aria-haspopup" "true"
            , attribute "aria-expanded" "false"
            ]
            [ text "More" ]
        , div
            [ class "dropdown-menu dropdown-menu-right", attribute "aria-labelledby" "dropdownMenuButton" ]
            [ dropDownItem DDAddBasicAuthentication
            , dropDownItem DDAddHeader
            , dropDownItem DDAddParameter
            , dropDownItem DDAddBody
            ]
        ]


dropDownItem : DropDownAction -> Html Msg
dropDownItem item =
    a
        [ class "dropdown-item"
        , href "javascript:void(0)"
        , onClick <| MoreActionsDropdownChange item
        ]
        [ text <| dropDownActionToString item ]


dropDownActionToString : DropDownAction -> String
dropDownActionToString dda =
    case dda of
        DDAddParameter ->
            "Add Parameter"

        DDAddHeader ->
            "Add Header"

        DDAddBody ->
            "Add Request Body"

        DDAddBasicAuthentication ->
            "Add Basic Authentication"


requestParametersView : Request -> Bool -> Html RequestParameters.Msg
requestParametersView { requestParameters } showErrors =
    if Dict.isEmpty requestParameters then
        text ""
    else
        RequestParameters.view requestParameters showErrors


requestHeadersView : Request -> Bool -> Html RequestHeaders.Msg
requestHeadersView { requestHeaders } showErrors =
    if Dict.isEmpty requestHeaders then
        text ""
    else
        RequestHeaders.view requestHeaders showErrors


requestBasicAuthenticationView : Request -> Html RequestBasicAuthentication.Msg
requestBasicAuthenticationView { basicAuthentication } =
    case basicAuthentication of
        Nothing ->
            text ""

        Just b ->
            RequestBasicAuthentication.view b


requestBodyView : Request -> Bool -> Html RequestBody.Msg
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
            , on "change" <| Json.Decode.map HttpMethodsDropdownChange targetValue
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
