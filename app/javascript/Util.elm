module Util exposing (..)


isMaybeValuePresent : Maybe a -> Bool
isMaybeValuePresent maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


isStringEmpty : String -> Bool
isStringEmpty =
    String.trim >> String.isEmpty


isStringPresent : String -> Bool
isStringPresent =
    isStringEmpty >> not
