module HttpUtil exposing (..)

import JSVal
import Json.Decode
import Http
import HttpMethods exposing (HttpMethod, parse, toString)
import Array
import Pages.Hit.RequestParameters exposing (..)


encodeUrl : String -> RequestParameters -> String
encodeUrl url requestParameters =
    requestParameters
        |> Array.filter (\parameter -> parameter.name /= "")
        |> Array.map (\parameter -> Http.encodeUri parameter.name ++ "=" ++ Http.encodeUri parameter.value)
        |> Array.toList
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
