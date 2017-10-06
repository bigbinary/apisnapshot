module Models exposing (..)

import Http
import HttpMethods exposing (HttpMethod)
import JsonViewer
import RequestParameters exposing (RequestParameters)
import Router exposing (..)


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


type alias Model =
    { url : String
    , httpMethod : HttpMethod
    , requestParameters : RequestParameters
    , pageState : PageState
    , route : Route
    }
