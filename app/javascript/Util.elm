module Util exposing (..)

import Date.Extra as Date


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


formatAndLocalizeDatetime : String -> String
formatAndLocalizeDatetime dateString =
    case Date.fromIsoString dateString of
        Just date ->
            Date.toFormattedString "ddd MMMM y, h:mm a" date

        Nothing ->
            dateString
