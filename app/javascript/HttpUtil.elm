module HttpUtil exposing (..)

import Dict exposing (Dict)
import JSVal
import Json.Decode as JD
import Json.Decode.Pipeline as JP
import Http
import HttpMethods exposing (HttpMethod, parse, toString)
import Models exposing (Request)
import Response exposing (Response)
import Pages.Hit.RequestParameters as RequestParameters exposing (..)
import Pages.Hit.RequestHeaders as RequestHeaders exposing (..)


encodeUrl : String -> RequestParameters -> String
encodeUrl url requestParameters =
    requestParameters
        |> Dict.values
        |> List.filter (\{ key } -> key /= "")
        |> List.map (\{ key, value } -> Http.encodeUri key ++ "=" ++ Http.encodeUri value)
        |> String.join "&"
        |> (++) (url ++ "?")


decodeHitResponseBodyIntoJson : Response -> JSVal.JSVal
decodeHitResponseBodyIntoJson hitResponse =
    let
        decoder =
            JD.at [ "response", "response_body" ] JSVal.decoder

        result =
            JD.decodeString decoder hitResponse.body

        error message =
            JSVal.JSString ("Error parsing the body. " ++ message)

        parseString string =
            let
                result =
                    JD.decodeString JSVal.decoder string
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


requestParametersDecoder : JD.Decoder RequestParameters
requestParametersDecoder =
    JD.keyValuePairs JD.string
        |> JD.andThen
            (\result ->
                result
                    |> List.map (\( key, value ) -> RequestParameter key value)
                    |> List.foldl (\item memo -> RequestParameters.push item memo) RequestParameters.empty
                    |> JD.succeed
            )


requestHeadersDecoder : JD.Decoder RequestHeaders
requestHeadersDecoder =
    JD.keyValuePairs JD.string
        |> JD.andThen
            (\result ->
                result
                    |> List.map (\( key, value ) -> RequestHeader key value)
                    |> List.foldl (\item memo -> RequestHeaders.push item memo) RequestHeaders.empty
                    |> JD.succeed
            )


httpMethodDecoder : JD.Decoder HttpMethod
httpMethodDecoder =
    JD.string
        |> JD.andThen (\str -> JD.succeed (parse str))


requestDecoder : JD.Decoder Request
requestDecoder =
    JP.decode Request
        |> JP.optional "url" JD.string ""
        |> JP.hardcoded Nothing
        |> JP.optional "httpMethod" httpMethodDecoder HttpMethods.Get
        |> JP.optional "requestParams" requestParametersDecoder RequestParameters.empty
        |> JP.optional "requestHeaders" requestHeadersDecoder RequestHeaders.empty


decodeHitResponseIntoRequest : Response -> Request
decodeHitResponseIntoRequest hitResponse =
    let
        result =
            JD.decodeString requestDecoder hitResponse.body
    in
        case result of
            Ok decodedValue ->
                decodedValue

            Err err ->
                Models.emptyRequest


decodeHeadersFromHitResponse : Response -> List ( String, String )
decodeHeadersFromHitResponse hitResponse =
    let
        decoder =
            JD.keyValuePairs JD.string
                |> JD.at [ "response", "response_headers" ]

        result =
            JD.decodeString decoder hitResponse.body
    in
        case result of
            Ok jsonValue ->
                jsonValue

            Err err ->
                []


decodeCreatedAtFromResponse : Response -> Maybe String
decodeCreatedAtFromResponse response =
    let
        decoder =
            JD.field "createdAt" JD.string

        result =
            JD.decodeString decoder response.body
    in
        case result of
            Ok date ->
                Just date

            Err err ->
                Nothing


decodeTokenFromResponse : Response -> Maybe String
decodeTokenFromResponse response =
    let
        decoder =
            JD.field "token" JD.string

        result =
            JD.decodeString decoder response.body
    in
        case result of
            Ok token ->
                Just token

            Err err ->
                Nothing


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
