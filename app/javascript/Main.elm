module Main exposing (..)

import HttpMethods exposing (HttpMethod(..))
import Models exposing (Model, Request, PageState(..))
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Pages.Hit.RequestParameters as RequestParameters exposing (RequestParameters)
import Router exposing (..)
import Update exposing (update)
import Models exposing (Model)
import Msgs exposing (Msg)
import Pages.Hit.Response
import Pages.Hit.Request
import Pages.Preferences
import Pages.NotFound
import Router exposing (..)
import Html exposing (Html, div, ul, li, a, text)
import Html.Attributes exposing (class, href)


initialModel : Route -> Model
initialModel route =
    let
        requestParameters =
            RequestParameters.empty
                |> RequestParameters.push { key = "name", value = "Sam" }
                |> RequestParameters.push { key = "age", value = "25" }
    in
        { request = Request "https://reqres.in/api/users" Nothing Post requestParameters
        , pageState = Empty
        , route = route
        }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            Router.parseLocation location
    in
        initialModel route ! []


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
    div [ class "container-fluid" ]
        [ div [] [ page model ]
        ]


page : Model -> Html Msg
page model =
    case model.route of
        Home ->
            div []
                [ div [ class "container-fluid api-req-form__container" ] [ Pages.Hit.Request.view model ]
                , Pages.Hit.Response.view model
                ]

        Preferences ->
            Pages.Preferences.view model

        NotFound ->
            Pages.NotFound.view model
