module Models exposing (..)

import HttpMethods exposing (HttpMethod)
import Pages.Hit.RequestParameters as RequestParameters exposing (RequestParameters)
import Pages.Hit.RequestHeaders as RequestHeaders exposing (RequestHeaders)
import RemoteData exposing (WebData)
import Response exposing (Response, ResponseViewing)
import JsonViewerTypes exposing (..)


type Route
    = HomeRoute
    | HitRoute String
    | NotFound


type alias Request =
    { url : String
    , httpMethod : HttpMethod
    , requestParameters : RequestParameters
    , requestHeaders : RequestHeaders
    }


emptyRequest : Request
emptyRequest =
    Request ""
        HttpMethods.Get
        RequestParameters.empty
        RequestHeaders.empty


type alias Model =
    { request : Request
    , response : WebData Response
    , route : Route
    , collapsedNodePaths : CollapsedNodePaths
    , responseViewing : ResponseViewing
    , showErrors : Bool
    }
