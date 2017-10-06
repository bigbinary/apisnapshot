module LocalStorageData exposing (..)


type LocalStorageData error value
    = Loading
    | Failure error
    | Success value
