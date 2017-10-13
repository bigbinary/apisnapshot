port module Ports exposing (..)

import Json.Encode exposing (Value)


port localStorageSet : { key : String, value : String } -> Cmd msg


port localStorageSetResponse : (String -> msg) -> Sub msg


port localStorageGet : String -> Cmd msg


port localStorageGetResponse : (String -> msg) -> Sub msg


port firebaseInitialize : String -> Cmd msg


port firebaseInitializeResponse : ({ success : Bool, error : String } -> msg) -> Sub msg


port firebaseSaveHit : Value -> Cmd msg


port firebaseSaveHitResponse : ({ uuid : String, error : String } -> msg) -> Sub msg
