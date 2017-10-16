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
        |> List.filter (\{ name } -> name /= "")
        |> List.map (\{ name, value } -> Http.encodeUri name ++ "=" ++ Http.encodeUri value)
        |> String.join "&"
        |> (++) (url ++ "?")


parseResponseBodyToJson : Http.Response String -> JSVal.JSVal
parseResponseBodyToJson httpResponse =
    let
        result =
            Json.Decode.decodeString JSVal.decoder httpResponse.body
    in
        case result of
            Ok jsonValue ->
                jsonValue

            Err err ->
                JSVal.JSString ("Error parsing the body. " ++ err)


buildRequest : String -> HttpMethod -> Http.Request (Http.Response String)
buildRequest url httpMethod =
    Http.request
        { method = HttpMethods.toString httpMethod
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse preserveFullResponse
        , timeout = Nothing
        , withCredentials = False
        }


preserveFullResponse : Http.Response String -> Result String (Http.Response String)
preserveFullResponse response =
    Ok response
