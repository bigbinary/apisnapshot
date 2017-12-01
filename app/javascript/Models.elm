module Models exposing (..)

import HttpMethods exposing (HttpMethod)
import Pages.Hit.RequestParameters as RequestParameters exposing (RequestParameters)
import RemoteData exposing (WebData)
import Response exposing (Response, ResponseViewing)
import JsonViewerTypes exposing (..)


type Route
    = HomeRoute
    | HitRoute String
    | NotFound


type alias Request =
    { url : String
    , urlError : Maybe String
    , httpMethod : HttpMethod
    , requestParameters : RequestParameters
    }


emptyRequest : Request
emptyRequest =
    Request "" Nothing HttpMethods.Get RequestParameters.empty


type alias Model =
    { request : Request
    , response : WebData Response
    , route : Route
    , collapsedNodePaths : CollapsedNodePaths
    , responseViewing : ResponseViewing
    }
