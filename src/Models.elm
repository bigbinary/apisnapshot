module Models exposing (..)

import Http
import HttpMethods exposing (HttpMethod)
import JsonViewer
import LocalStorageData exposing (..)
import RequestParameters exposing (RequestParameters)
import Router exposing (..)


firebaseConfigLocalStorageKey : String
firebaseConfigLocalStorageKey =
    "firebaseConfig"


type alias Response =
    { raw : Http.Response String
    , collapsedNodePaths : JsonViewer.CollapsedNodePaths
    , json : JsonViewer.JsonView
    }


type PageState
    = Empty
    | Loading
    | Error Http.Error
    | Loaded Response


type FirebaseSdkInitializationState
    = Initializing
    | Intialized
    | InitializationError String


type alias FirebaseConfig =
    { apiKey : String
    , authDomain : String
    , databaseURL : String
    , projectId : String
    , storageBucket : String
    , messagingSenderId : String
    }


type alias Model =
    { url : String
    , error : Maybe String
    , httpMethod : HttpMethod
    , requestParameters : RequestParameters
    , pageState : PageState
    , route : Route
    , firebaseSdkInitializationState : FirebaseSdkInitializationState
    , firebaseConfig : LocalStorageData String FirebaseConfig
    , dirtyFirebaseConfig : FirebaseConfig
    }
