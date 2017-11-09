module HttpUtil exposing (..)

import Dict exposing (Dict)
import JSVal
import Json.Decode
import Http
import HttpMethods exposing (HttpMethod, parse, toString)
import Pages.Hit.RequestParameters exposing (..)


encodeUrl : String -> RequestParameters -> String
encodeUrl url requestParameters =
    requestParameters
        |> Dict.values
        |> List.filter (\{ key } -> key /= "")
        |> List.map (\{ key, value } -> Http.encodeUri key ++ "=" ++ Http.encodeUri value)
        |> String.join "&"
        |> (++) (url ++ "?")


parseResponseBodyToJson : String -> JSVal.JSVal
parseResponseBodyToJson httpResponseBody =
    let
        result =
            Json.Decode.decodeString (Json.Decode.at [ "response", "body" ] JSVal.decoder) httpResponseBody

        error message =
            JSVal.JSString ("Error parsing the body. " ++ message)

        parseString string =
            let
                result =
                    Json.Decode.decodeString JSVal.decoder string
            in
                case result of
                    Ok jsonValue ->
                        jsonValue

                    Err err ->
                        error err
    in
        case result of
            Ok jsonValue ->
                case jsonValue of
                    JSVal.JSString stringValue ->
                        parseString stringValue

                    _ ->
                        jsonValue

            Err err ->
                error err


parseResponseHeaders : String -> List ( String, String )
parseResponseHeaders httpResponseBody =
    let
        decoder =
            Json.Decode.keyValuePairs Json.Decode.string
                |> Json.Decode.field "response_headers"

        result =
            Json.Decode.decodeString decoder httpResponseBody
    in
        case result of
            Ok jsonValue ->
                jsonValue

            Err err ->
                []


buildRequest : String -> HttpMethod -> Http.Body -> Http.Request (Http.Response String)
buildRequest url httpMethod body =
    Http.request
        { method = HttpMethods.toString httpMethod
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectStringResponse preserveFullResponse
        , timeout = Nothing
        , withCredentials = False
        }


preserveFullResponse : Http.Response String -> Result String (Http.Response String)
preserveFullResponse response =
    Ok response
