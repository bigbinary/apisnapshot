module Utils.Util exposing (..)


isStringPresent : String -> Bool
isStringPresent s =
    case (String.trim s) of
        "" ->
            False

        _ ->
            True
