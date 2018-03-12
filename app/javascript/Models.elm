module Models exposing (..)

import HttpMethods exposing (HttpMethod)
import Pages.Hit.RequestParameters as RequestParameters exposing (RequestParameters)
import Pages.Hit.RequestHeaders as RequestHeaders exposing (RequestHeaders)
import RemoteData exposing (WebData)
import Response exposing (Response, ResponseViewing)
import JsonViewerTypes exposing (..)
import Util exposing (RequestBody)

type Route
    = HomeRoute
    | HitRoute String
    | NotFound


type alias Request =
    { url : String
    , httpMethod : HttpMethod
    , requestParameters : RequestParameters
    , requestHeaders : RequestHeaders
    , requestBody : Maybe RequestBody
    }


emptyRequest : Request
emptyRequest =
    Request ""
        HttpMethods.Get
        RequestParameters.empty
        RequestHeaders.empty
        Nothing


type alias Model =
    { request : Request
    , response : WebData Response
    , route : Route
    , collapsedNodePaths : CollapsedNodePaths
    , responseViewing : ResponseViewing
    , showErrors : Bool
    }
