module View exposing (view)

import Html exposing (Html, div)
import Layout
import Models exposing (Model)
import Msgs exposing (Msg)
import Pages.Home
import Pages.Preferences
import Pages.NotFound
import Router exposing (..)


view : Model -> Html Msg
view model =
    div [] [ page model ] |> Layout.embed model


page : Model -> Html Msg
page model =
    case model.route of
        Home ->
            Pages.Home.view model

        Preferences ->
            Pages.Preferences.view model

        NotFound ->
            Pages.NotFound.view model
