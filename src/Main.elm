module Main exposing (..)

import HttpMethods exposing (HttpMethod(..))
import Models exposing (PageState(..), Model)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import RequestParameters exposing (empty)
import Router exposing (..)
import Update exposing (update)
import View exposing (view)


initialModel : Route -> Model
initialModel route =
    { url = "https://swapi.co/api/people/1/"
    , httpMethod = Get
    , requestParameters = empty
    , pageState = Empty
    , route = route
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            Router.parseLocation location
    in
        ( initialModel route, Cmd.none )


main : Program Never Model Msg
main =
    Navigation.program Msgs.OnLocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
