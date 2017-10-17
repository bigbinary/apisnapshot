module Models exposing (..)

import Http
import HttpMethods exposing (HttpMethod)
import JsonViewer
import LocalStorageData exposing (..)
import Pages.Hit.RequestParameters exposing (RequestParameters)
import Router exposing (..)
import Tuple


firebaseConfigLocalStorageKey : String
firebaseConfigLocalStorageKey =
    "firebaseConfig"


type ResponseViewing
    = Formatted
    | Raw


type alias Response =
    { raw : Http.Response String
    , collapsedNodePaths : JsonViewer.CollapsedNodePaths
    , json : JsonViewer.JsonView
    , headers : List ( String, String )
    , viewing : ResponseViewing
    }


type PageState
    = Empty
    | Loading
    | Error Http.Error
    | Loaded Response


type alias FirebaseConfig =
    { apiKey : String
    , authDomain : String
    , databaseURL : String
    , projectId : String
    , storageBucket : String
    , messagingSenderId : String
    }


type alias Request =
    { url : String
    , urlError : Maybe String
    , httpMethod : HttpMethod
    , requestParameters : RequestParameters
    }


type alias Model =
    { request : Request
    , pageState : PageState
    , route : Route
    , firebaseConfig : LocalStorageData String FirebaseConfig
    , dirtyFirebaseConfig : FirebaseConfig
    }
