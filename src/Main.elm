module Main exposing (..)

import HttpMethods exposing (HttpMethod(..))
import LocalStorageData exposing (..)
import Models exposing (FirebaseConfig, Model, PageState(..), firebaseConfigLocalStorageKey)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Ports exposing (..)
import Pages.Hit.RequestParameters exposing (empty)
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
    { url = "https://swapi.co/api/people/1/"
    , error = Nothing
    , httpMethod = Get
    , requestParameters = empty
    , pageState = Empty
    , route = route
    , firebaseConfig = LocalStorageData.Loading
    , dirtyFirebaseConfig = initialFirebaseConfig
    }


initialFirebaseConfig : FirebaseConfig
initialFirebaseConfig =
    FirebaseConfig "" "" "" "" "" ""


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            Router.parseLocation location
    in
        ( initialModel route
        , Ports.localStorageGet firebaseConfigLocalStorageKey
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.localStorageSetResponse Msgs.OnLocalStorageSet
        , Ports.localStorageGetResponse Msgs.OnLocalStorageGet
        ]


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
            Pages.Preferences.view model.dirtyFirebaseConfig |> Html.map Msgs.PreferencesMsg

        NotFound ->
            Pages.NotFound.view model
