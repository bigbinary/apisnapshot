module Pages.Hit.RequestBody exposing(..)

import Html exposing(..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Msgs exposing (Msg)
import Util exposing (..)
import Json.Encode exposing (..)

emptyBody = 
    { bodyType = BodyText
    , value = ""
    }

view bodyString bodyType showErrors =
    div [ class "form-row" ]
        [ div [class "form-group__label"] 
            [ text "Request Body: "
            , select [class "api-req-form__request-bod", onInput (onSelect bodyString)]
                [ option [isTextSelected bodyType, value "text" ] [text "Text"]
                , option [isJsonSelected bodyType, value "json" ] [text "JSON"]
                ]
            , a [ class "btn devise-links", onClick removeBody, href "javascript:void(0)"] [text "Remove Request Body"]
            ]
        , textarea [class "form-control api-req-form__textarea", rows 8, cols 8
            , onInput (\val -> Msgs.UpdateRequestBody {bodyType = bodyType, value = val})
            ] [ text bodyString ]
        ]

onSelect strngVal val =
    case val of
        "text" ->
            Msgs.UpdateRequestBody {bodyType = BodyText, value = strngVal}
    
        "json" ->
            Msgs.UpdateRequestBody {bodyType = BodyJSON, value = strngVal}

        _ -> Msgs.UpdateRequestBody {bodyType = BodyText, value = strngVal}

removeBody =
    Msgs.RemoveRequestBody

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
        [ ( "bodyType", (requestBodyTypeEncode rb.bodyType))
        , ( "value", string rb.value)
        ]

requestBodyTypeEncode : RequestBodyType -> Value
requestBodyTypeEncode rbt = 
    case rbt of
        BodyJSON -> string "BodyJSON"
        BodyText -> string "BodyText"
            
