module JSVal exposing (..)

import Json.Decode as Decode exposing (Decoder, Value)


type JSVal
    = JSString String
    | JSFloat Float
    | JSInt Int
    | JSBool Bool
    | JSNull
    | JSArray (List JSVal)
    | JSObject (List ( String, JSVal ))


decoder : Decoder JSVal
decoder =
    Decode.oneOf
        [ Decode.map JSString Decode.string
        , Decode.map JSInt Decode.int
        , Decode.map JSFloat Decode.float
        , Decode.map JSBool Decode.bool
        , Decode.null JSNull
        , Decode.map JSArray (Decode.list (Decode.lazy <| \_ -> decoder))
        , Decode.map (List.reverse >> JSObject) (Decode.keyValuePairs (Decode.lazy <| \_ -> decoder))
        ]
