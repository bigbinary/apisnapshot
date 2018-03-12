module Util exposing (..)

import Date.Extra as Date

type RequestBodyType 
    = BodyJSON
    | BodyText

type alias RequestBody =
    { bodyType : RequestBodyType
    , value : String 
    }

type DropDownAction =
    DDAddParameter
    | DDAddHeader
    | DDAddBody


dropDownActionToString : DropDownAction -> String
dropDownActionToString dda =
    case dda of
        DDAddParameter ->
            "Add Parameter"
    
        DDAddHeader ->
            "Add Header"
        
        DDAddBody ->
            "Add Request Body"


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
