module Utils.HttpUtil exposing (..)

import Dict exposing (Dict)
import Response.JSVal as JSVal
import Json.Decode as JD
import Json.Decode.Pipeline as JP
import Http
import Utils.HttpMethods as HttpMethods exposing (HttpMethod, parse, toString)
import Request.RequestParameters as RequestParameters exposing (..)
import Request.RequestHeaders as RequestHeaders exposing (..)
import Request.RequestBody as RequestHeaders exposing (..)
import Request.RequestBasicAuthentication as RequestBasicAuthentication exposing (..)


type alias Response =
    Http.Response String


type alias Request =
    { url : String
    , httpMethod : HttpMethod
    , requestParameters : RequestParameters
    , requestHeaders : RequestHeaders
    , basicAuthentication : Maybe BasicAuthentication
    , requestBody : Maybe RequestBody
    }


type alias SRequest =
    { url : String
    , httpMethod : HttpMethod
    , requestParameters : RequestParameters
    , requestHeaders : RequestHeaders
    , username : Maybe String
    , password : Maybe String
    , requestBody : Maybe RequestBody
    }


emptyRequest : Request
emptyRequest =
    Request ""
        HttpMethods.Get
        RequestParameters.empty
        RequestHeaders.empty
        Nothing
        Nothing


encodeUrl : String -> RequestParameters -> String
encodeUrl url requestParameters =
    requestParameters
        |> Dict.values
        |> List.filter (\{ key } -> key /= "")
        |> List.map (\{ key, value } -> Http.encodeUri key ++ "=" ++ Http.encodeUri value)
        |> String.join "&"
        |> (++) (url ++ "?")


decodeResponseBodyToString : Response -> String
decodeResponseBodyToString hitResponse =
    let
        result =
            JD.decodeString (JD.at [ "response", "response_body" ] JD.string) hitResponse.body
    in
        case result of
            Ok value ->
                value

            Err err ->
                ("Error: Could not parse body. " ++ err)


decodeHitResponseBodyIntoJson : Response -> JSVal.JSVal
decodeHitResponseBodyIntoJson hitResponse =
    let
        result =
            JD.decodeString (JD.at [ "response", "response_body" ] JSVal.decoder) hitResponse.body

        errorParsing message =
            JSVal.JSString ("Error: Could not parse body. " ++ message)

        parseString string =
            let
                result =
                    JD.decodeString JSVal.decoder string
            in
                case result of
                    Ok jsonValue ->
                        jsonValue

                    Err err ->
                        errorParsing err
    in
        case result of
            Ok jsonValue ->
                case jsonValue of
                    JSVal.JSString stringValue ->
                        parseString stringValue

                    _ ->
                        jsonValue

            Err err ->
                errorParsing err


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


requestBasicAuthenticationDecoder : JD.Decoder BasicAuthentication
requestBasicAuthenticationDecoder =
    JP.decode BasicAuthentication
        |> JP.required "username" JD.string
        |> JP.required "password" JD.string


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


requestDecoder : JD.Decoder SRequest
requestDecoder =
    JP.decode SRequest
        |> JP.optional "url" JD.string ""
        |> JP.optional "httpMethod" httpMethodDecoder HttpMethods.Get
        |> JP.optional "requestParams" requestParametersDecoder RequestParameters.empty
        |> JP.optional "requestHeaders" requestHeadersDecoder RequestHeaders.empty
        |> JP.optional "username" (JD.map Just JD.string) Nothing
        |> JP.optional "password" (JD.map Just JD.string) Nothing
        |> JP.optional "requestBody"
            (JD.map Just requestBodyDecoder)
            Nothing


requestBodyDecoder : JD.Decoder RequestBody
requestBodyDecoder =
    JP.decode RequestBody
        |> JP.required "bodyType" requestBodyTypeDecoder
        |> JP.required "value" JD.string


requestBodyTypeDecoder : JD.Decoder RequestBodyType
requestBodyTypeDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case str of
                    "BodyText" ->
                        JD.succeed BodyText

                    "BodyJSON" ->
                        JD.succeed BodyJSON

                    somethingElse ->
                        JD.fail <| "Unknown type: " ++ somethingElse
            )


decodeHitResponseIntoRequest : Response -> Request
decodeHitResponseIntoRequest hitResponse =
    let
        result =
            JD.decodeString requestDecoder hitResponse.body
    in
        case result of
            Ok v ->
                let
                    extractedCreds =
                        ( v.username, v.password )

                    bA =
                        case extractedCreds of
                            ( Nothing, Nothing ) ->
                                Nothing

                            ( Just a, Nothing ) ->
                                Just { username = a, password = "" }

                            ( Nothing, Just a ) ->
                                Just { username = "", password = a }

                            ( Just u, Just p ) ->
                                Just { username = u, password = p }
                in
                    { url = v.url
                    , httpMethod = v.httpMethod
                    , requestParameters = v.requestParameters
                    , requestHeaders = v.requestHeaders
                    , basicAuthentication = bA
                    , requestBody = v.requestBody
                    }

            Err err ->
                emptyRequest


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


decodeStatusCodeFromResponse : Response -> Int
decodeStatusCodeFromResponse response =
    let
        decoder =
            JD.string |> JD.at [ "response", "response_code" ]

        result =
            JD.decodeString decoder response.body

        defaultStatusCode =
            500
    in
        case result of
            Ok statusCode ->
                String.toInt statusCode |> Result.withDefault defaultStatusCode

            Err err ->
                defaultStatusCode


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
