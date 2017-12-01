module Response exposing (..)

import Http


type ResponseViewing
    = Formatted
    | Raw


type alias Response =
    Http.Response String
