module Main exposing (..)

import Html exposing (program)
import HttpMethods exposing (HttpMethod(..))
import Models exposing (PageState(..), Model)
import Msgs exposing (Msg)
import RequestParameters exposing (empty)
import Update exposing (update)
import View exposing (view)


init : ( Model, Cmd Msg )
init =
    ( { url = "https://swapi.co/api/people/1/"
      , httpMethod = Get
      , requestParameters = empty
      , pageState = Empty
      }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
