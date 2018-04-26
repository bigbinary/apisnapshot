module Request.RequestBody exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode exposing (..)


-- MODEL --


type RequestBodyType
    = BodyJSON
    | BodyText


type alias RequestBody =
    { bodyType : RequestBodyType
    , value : String
    }


emptyBody =
    { bodyType = BodyText
    , value = ""
    }



--  UPDATE --


type Msg
    = UpdateRequestBody RequestBody
    | RemoveRequestBody


update : Msg -> Maybe RequestBody -> ( Maybe RequestBody, Cmd Msg )
update msg model =
    case msg of
        UpdateRequestBody val ->
            let
                requestBody =
                    Just val
            in
                ( requestBody, Cmd.none )

        RemoveRequestBody ->
            let
                requestBody =
                    Nothing
            in
                ( requestBody, Cmd.none )



-- VIEW --


view bodyString bodyType showErrors =
    div [ class "form-row" ]
        [ div [ class "form-group__label" ]
            [ text "Request Body: "
            , select [ class "api-req-form__request-bod", onInput (onSelect bodyString) ]
                [ option [ isTextSelected bodyType, value "text" ] [ text "Text" ]
                , option [ isJsonSelected bodyType, value "json" ] [ text "JSON" ]
                ]
            , a [ class "btn devise-links", onClick RemoveRequestBody, href "javascript:void(0)" ] [ text "Remove Request Body" ]
            ]
        , textarea
            [ class "form-control api-req-form__textarea"
            , rows 8
            , cols 8
            , onInput (\val -> UpdateRequestBody { bodyType = bodyType, value = val })
            ]
            [ text bodyString ]
        ]


onSelect strngVal val =
    case val of
        "text" ->
            UpdateRequestBody { bodyType = BodyText, value = strngVal }

        "json" ->
            UpdateRequestBody { bodyType = BodyJSON, value = strngVal }

        _ ->
            UpdateRequestBody { bodyType = BodyText, value = strngVal }


isTextSelected bodyType =
    case bodyType of
        BodyJSON ->
            selected False

        BodyText ->
            selected True


isJsonSelected bodyType =
    case bodyType of
        BodyJSON ->
            selected True

        BodyText ->
            selected False


requestBodyEncoder : RequestBody -> Value
requestBodyEncoder rb =
    Json.Encode.object
        [ ( "bodyType", (requestBodyTypeEncode rb.bodyType) )
        , ( "value", string rb.value )
        ]


requestBodyTypeEncode : RequestBodyType -> Value
requestBodyTypeEncode rbt =
    case rbt of
        BodyJSON ->
            string "BodyJSON"

        BodyText ->
            string "BodyText"
