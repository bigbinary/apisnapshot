module Request.RequestParameters exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode exposing (..)
import Utils.Util exposing (isStringPresent)


type alias RequestParameter =
    { key : String
    , value : String
    }


type alias Position =
    Int


type alias RequestParameters =
    Dict Position RequestParameter


blankRequestParameter : RequestParameter
blankRequestParameter =
    RequestParameter "" ""


empty : RequestParameters
empty =
    Dict.empty



-- UPDATE --


type Msg
    = AddRequestParameter
    | ChangeRequestParameterName Int String
    | ChangeRequestParameterValue Int String
    | DeleteRequestParameter Int


addParameters rp =
    let
        newRequestParameters =
            pushBlank rp
    in
        ( newRequestParameters, Cmd.none )


update msg model =
    case msg of
        AddRequestParameter ->
            addParameters model

        ChangeRequestParameterName index newName ->
            let
                newRequestParameters =
                    updateName index newName model
            in
                ( newRequestParameters, Cmd.none )

        ChangeRequestParameterValue index newValue ->
            let
                newRequestParameters =
                    updateValue index newValue model
            in
                ( newRequestParameters, Cmd.none )

        DeleteRequestParameter index ->
            let
                newRequestParameters =
                    remove index model
            in
                ( newRequestParameters, Cmd.none )


pushBlank : RequestParameters -> RequestParameters
pushBlank requestParameters =
    push blankRequestParameter requestParameters


push : RequestParameter -> RequestParameters -> RequestParameters
push requestParameter requestParameters =
    Dict.insert (Dict.size requestParameters) requestParameter requestParameters


updateName : Position -> String -> RequestParameters -> RequestParameters
updateName position newName requestParameters =
    let
        requestParameter =
            Dict.get position requestParameters

        newRequestParameter =
            case requestParameter of
                Just requestParameter_ ->
                    { requestParameter_ | key = newName }

                Nothing ->
                    RequestParameter newName ""
    in
        updateRequestParameter position newRequestParameter requestParameters


updateValue : Position -> String -> RequestParameters -> RequestParameters
updateValue position newValue requestParameters =
    let
        requestParameter =
            Dict.get position requestParameters

        newRequestParameter =
            case requestParameter of
                Just requestParameter_ ->
                    { requestParameter_ | value = newValue }

                Nothing ->
                    RequestParameter newValue ""
    in
        updateRequestParameter position newRequestParameter requestParameters


updateRequestParameter : Position -> RequestParameter -> RequestParameters -> RequestParameters
updateRequestParameter position newRequestParameter requestParameters =
    Dict.update position (\_ -> Just newRequestParameter) requestParameters


remove : Position -> RequestParameters -> RequestParameters
remove position requestParameters =
    Dict.remove position requestParameters
        |> Dict.foldl
            (\_ requestParameter newRequestParameters ->
                Dict.insert (Dict.size newRequestParameters) requestParameter newRequestParameters
            )
            Dict.empty



-- VIEW --


viewRequestParameter : Bool -> Position -> RequestParameter -> Html Msg
viewRequestParameter showErrors position requestParameter =
    div [ class "form-row" ]
        [ viewRequestParameterName position requestParameter showErrors
        , div [ class "col" ]
            [ input
                [ type_ "text"
                , placeholder "Enter Value"
                , class "input form-control api-req-form__input"
                , value requestParameter.value
                , onInput (ChangeRequestParameterValue position)
                ]
                []
            ]
        , div [ class "col" ]
            [ a
                [ href "javascript:void(0)"
                , class "RequestParameters__delete"
                , onClick (DeleteRequestParameter position)
                ]
                [ text "×" ]
            ]
        ]


viewRequestParameterName : Position -> RequestParameter -> Bool -> Html Msg
viewRequestParameterName position { key } showErrors =
    let
        defaultClass =
            "input form-control api-req-form__input"

        shouldShowError =
            showErrors && not (isStringPresent key)

        updatedClass =
            if shouldShowError then
                defaultClass ++ " is-invalid"
            else
                defaultClass

        viewValidationError =
            if shouldShowError then
                div [ class "invalid-feedback" ] [ text "Cannot be empty" ]
            else
                text ""
    in
        div [ class "col" ]
            [ input
                [ type_ "text"
                , placeholder "Enter Name"
                , class updatedClass
                , value key
                , onInput (ChangeRequestParameterName position)
                ]
                []
            , viewValidationError
            ]


viewRequestParameters : RequestParameters -> Bool -> Html Msg
viewRequestParameters requestParameters showErrors =
    div [ class "aapi-req-form__form-inline" ]
        (requestParameters
            |> Dict.map (viewRequestParameter showErrors)
            |> Dict.toList
            |> List.map (\( _, viewRequestParameter ) -> viewRequestParameter)
        )


view : RequestParameters -> Bool -> Html Msg
view requestParameters showErrors =
    div [ class "form-group" ]
        [ div [ class "form-group__label" ]
            [ span [] [ text "Request Parameters" ]
            , a [ href "javascript:void(0)", class "devise-links", onClick AddRequestParameter ] [ text "Add Parameter" ]
            ]
        , viewRequestParameters requestParameters showErrors
        ]



-- UTILITY FUNCTIONS


valid : RequestParameters -> Bool
valid requestParameters =
    requestParameters
        |> Dict.values
        |> List.map (\{ key } -> isStringPresent key)
        |> List.member False
        |> not



-- ENCODERS


requestParametersEncoder : RequestParameters -> Value
requestParametersEncoder requestParamters =
    Dict.map requestParameterEncoder requestParamters
        |> Dict.toList
        |> List.map (\( key, value ) -> ( toString key, value ))
        |> Json.Encode.object


requestParameterEncoder : Int -> RequestParameter -> Value
requestParameterEncoder index requestParameter =
    Json.Encode.object
        [ ( "key", string requestParameter.key )
        , ( "value", string requestParameter.value )
        ]
