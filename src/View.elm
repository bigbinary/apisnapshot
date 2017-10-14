module View exposing (view)

import Html exposing (Html, div)
import Models exposing (Model)
import Msgs exposing (Msg)
import Pages.Home
import Pages.Hit.Response
import Pages.Preferences
import Pages.NotFound
import Router exposing (..)
import Html exposing (Html, div, ul, li, a, text)
import Html.Attributes exposing (class, href)


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
                [ div [ class "container-fluid api-req-form__container" ] [ Pages.Home.view model ]
                , Pages.Hit.Response.view model
                ]

        Preferences ->
            Pages.Preferences.view model.dirtyFirebaseConfig |> Html.map Msgs.PreferencesMsg

        NotFound ->
            Pages.NotFound.view model
