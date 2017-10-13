module HitEncoder exposing (..)

import Array exposing (Array)
import HttpMethods
import Json.Encode exposing (..)
import Models exposing (..)
import RequestParameters exposing (RequestParameters, RequestParameter)


encode : Model -> Value
encode model =
    let
        attributes =
            [ ( "url", string model.url )
            , ( "httpMethod", string <| HttpMethods.toString <| model.httpMethod )
            , ( "requestParameters", requestParametersEncoder model.requestParameters )
            , ( "pageState", pageStateEncoder model.pageState )
            ]
    in
        object attributes


requestParametersEncoder : RequestParameters -> Value
requestParametersEncoder parameters =
    Array.map requestParameterEncoder parameters |> array


requestParameterEncoder : RequestParameter -> Value
requestParameterEncoder parameter =
    object
        [ ( "name", string parameter.name )
        , ( "value", string parameter.value )
        ]


pageStateEncoder : PageState -> Value
pageStateEncoder pageState =
    case pageState of
        Error error ->
            object [ ( "error", string <| toString <| error ) ]

        Loaded response ->
            object [ ( "success", string response.raw.body ) ]

        _ ->
            object []
