module HttpMethods exposing (..)


type HttpMethod
    = Get
    | Post
    | Put
    | Patch
    | Delete


toString : HttpMethod -> String
toString httpMethod =
    case httpMethod of
        Get ->
            "GET"

        Post ->
            "POST"

        Put ->
            "PUT"

        Patch ->
            "PATCH"

        Delete ->
            "DELETE"


parse : String -> HttpMethod
parse httpMethodString =
    case httpMethodString of
        "GET" ->
            Get

        "POST" ->
            Post

        "PUT" ->
            Put

        "PATCH" ->
            Patch

        "DELETE" ->
            Delete

        _ ->
            Get


avaialableHttpMethods : List HttpMethod
avaialableHttpMethods =
    [ Get, Post, Put, Patch, Delete ]


avaialableHttpMethodsString : List String
avaialableHttpMethodsString =
    List.map toString avaialableHttpMethods
