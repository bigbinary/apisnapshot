module Router exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)

type Route
    = HomeRoute
    | HitRoute String
    | NotFound

matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map HomeRoute top
        , map HitRoute (s "hits" </> string)
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFound
