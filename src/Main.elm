module Main exposing (..)

import HttpMethods exposing (HttpMethod(..))
import LocalStorageData exposing (..)
import Models exposing (PageState(..), Model, FirebaseConfig, firebaseConfigLocalStorageKey)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Ports exposing (..)
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
