module Pages.Hit.RequestHeaders
    exposing
        ( RequestHeader
        , RequestHeaders
        , empty
        , push
        , pushBlank
        , remove
        , requestHeadersEncoder
        , updateName
        , updateValue
        , valid
        , view
        )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode exposing (..)
import Msgs exposing (Msg)
import Util exposing (isStringPresent)


-- TYPES --


type alias RequestHeader =
    { key : String
    , value : String
    }


type alias Position =
    Int


type alias RequestHeaders =
    Dict Position RequestHeader



-- CONSTANTS --


blankRequestHeader : RequestHeader
blankRequestHeader =
    RequestHeader "" ""


empty : RequestHeaders
empty =
    Dict.empty



-- ENCODERS


requestHeadersEncoder : RequestHeaders -> Value
requestHeadersEncoder requestParamters =
    Dict.map requestHeaderEncoder requestParamters
        |> Dict.toList
        |> List.map (\( key, value ) -> ( toString key, value ))
        |> Json.Encode.object


requestHeaderEncoder : Int -> RequestHeader -> Value
requestHeaderEncoder index requestHeader =
    Json.Encode.object
        [ ( "key", string requestHeader.key )
        , ( "value", string requestHeader.value )
        ]



-- METHODS --


pushBlank : RequestHeaders -> RequestHeaders
pushBlank requestHeaders =
    push blankRequestHeader requestHeaders


push : RequestHeader -> RequestHeaders -> RequestHeaders
push requestHeader requestHeaders =
    Dict.insert (Dict.size requestHeaders) requestHeader requestHeaders


updateName : Position -> String -> RequestHeaders -> RequestHeaders
updateName position newName requestHeaders =
    let
        requestHeader =
            Dict.get position requestHeaders

        newRequestHeader =
            case requestHeader of
                Just requestHeader_ ->
                    { requestHeader_ | key = newName }

                Nothing ->
                    RequestHeader newName ""
    in
        updateRequestHeader position newRequestHeader requestHeaders


updateValue : Position -> String -> RequestHeaders -> RequestHeaders
updateValue position newValue requestHeaders =
    let
        requestHeader =
            Dict.get position requestHeaders

        newRequestHeader =
            case requestHeader of
                Just requestHeader_ ->
                    { requestHeader_ | value = newValue }

                Nothing ->
                    RequestHeader newValue ""
    in
        updateRequestHeader position newRequestHeader requestHeaders


updateRequestHeader : Position -> RequestHeader -> RequestHeaders -> RequestHeaders
updateRequestHeader position newRequestHeader requestHeaders =
    Dict.update position (\_ -> Just newRequestHeader) requestHeaders


remove : Position -> RequestHeaders -> RequestHeaders
remove position requestHeaders =
    Dict.remove position requestHeaders
        |> Dict.foldl
            (\_ requestHeader newRequestHeaders ->
                Dict.insert (Dict.size newRequestHeaders) requestHeader newRequestHeaders
            )
            Dict.empty



-- VIEW --
-- , div [ class "col" ]
--             [ input
--                 [ type_ "text"
--                 , placeholder "Enter Value"
--                 , class "input form-control api-req-form__input"
--                 , value requestHeader.value
--                 , onInput (Msgs.ChangeRequestHeaderValue position)
--                 ]
--                 []
--             ]


viewRequestHeader : Position -> RequestHeader -> Html Msg
viewRequestHeader position requestHeader =
    div [ class "form-row" ]
        [ viewRequestHeaderAttribute "Name" position requestHeader.key
        , viewRequestHeaderAttribute "Value" position requestHeader.value
        , div [ class "col" ]
            [ a
                [ href "javascript:void(0)"
                , class "RequestHeaders__delete"
                , onClick (Msgs.DeleteRequestHeader position)
                ]
                [ text "×" ]
            ]
        ]


viewRequestHeaderAttribute : String -> Position -> String -> Html Msg
viewRequestHeaderAttribute label position value_ =
    let
        defaultClass =
            "input form-control api-req-form__input"

        updatedClass =
            if isStringPresent value_ then
                defaultClass
            else
                defaultClass ++ " is-invalid"

        viewValidationError =
            if isStringPresent value_ then
                text ""
            else
                div [ class "invalid-feedback" ]
                    [ text "Cannot be empty" ]
    in
        div [ class "col" ]
            [ input
                [ type_ "text"
                , placeholder ("Enter " ++ label)
                , class updatedClass
                , value value_
                , onInput (Msgs.ChangeRequestHeaderAttribute label position)
                ]
                []
            , viewValidationError
            ]


viewRequestHeaders : RequestHeaders -> Html Msg
viewRequestHeaders requestHeaders =
    div [ class "aapi-req-form__form-inline" ]
        (requestHeaders
            |> Dict.map viewRequestHeader
            |> Dict.toList
            |> List.map (\( _, viewRequestHeader ) -> viewRequestHeader)
        )


view : RequestHeaders -> Html Msg
view requestHeaders =
    div [ class "form-group" ]
        [ div [ class "form-group__label" ]
            [ span [] [ text "Request Headers" ]
            , a [ href "javascript:void(0)", class "devise-links", onClick Msgs.AddRequestHeader ] [ text "Add Header" ]
            ]
        , viewRequestHeaders requestHeaders
        ]



-- UTILITY FUNCTIONS


valid : RequestHeaders -> Bool
valid requestHeaders =
    requestHeaders
        |> Dict.values
        |> List.map (\{ key, value } -> isStringPresent key && isStringPresent value)
        |> List.member False
        |> not
