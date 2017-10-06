module Router exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)


type Route
    = Home
    | Preferences
    | NotFound


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Home top
        , map Preferences (s "preferences")
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFound
