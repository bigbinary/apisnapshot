port module Ports exposing (..)


port localStorageSet : { key : String, value : String } -> Cmd msg


port localStorageSetResponse : (String -> msg) -> Sub msg


port localStorageGet : String -> Cmd msg


port localStorageGetResponse : (String -> msg) -> Sub msg


port firebaseInitialize : String -> Cmd msg


port firebaseInitializeResponse : ({ success : Bool, error : String } -> msg) -> Sub msg
