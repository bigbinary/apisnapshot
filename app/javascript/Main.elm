module Main exposing (..)

import Models exposing (Model, Request)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Router exposing (..)
import Update exposing (update, updateRoute)
import Models exposing (Model, emptyRequest)
import Msgs exposing (Msg)
import Pages.Hit.Response
import Pages.Hit.Request
import Pages.NotFound
import Router exposing (..)
import Html exposing (Html, div, ul, li, a, text)
import Html.Attributes exposing (class, href)
import RemoteData
import Response
import Set


initialModel : Models.Route -> Model
initialModel route =
    { request = emptyRequest
    , response = RemoteData.NotAsked
    , responseViewing = Response.Formatted
    , collapsedNodePaths = Set.empty
    , route = route
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            Router.parseLocation location
    in
        updateRoute route (initialModel route)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Navigation.program Msgs.OnLocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    let
        homeRouteMarkup =
            div [] [ Pages.Hit.Request.view model, Pages.Hit.Response.view model ]
    in
        case model.route of
            Models.HomeRoute ->
                homeRouteMarkup

            Models.HitRoute _ ->
                homeRouteMarkup

            _ ->
                Pages.NotFound.view model
