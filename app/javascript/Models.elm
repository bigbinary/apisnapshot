module Models exposing (..)

import Http
import HttpMethods exposing (HttpMethod)
import JsonViewer
import Pages.Hit.RequestParameters exposing (RequestParameters)
import Router exposing (..)


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
    }
