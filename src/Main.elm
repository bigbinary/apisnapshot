module Main exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


---- MODEL ----


type alias BBJson =
    { name : String }


type alias DecodedResponse =
    { contents : Maybe BBJson
    , status :
        { code : Int
        , message : String
        }
    , headers : Dict.Dict String String
    }


type alias Response =
    { url : String
    , error : String
    , rawResponse : String
    , response : Maybe DecodedResponse
    }


type alias Model =
    { url : String, response : Maybe Response }


init : ( Model, Cmd Msg )
init =
    ( { url = "https://swapi.co/api/people/1/", response = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = Submit
    | ChangeUrl String
    | UrlMsg (Result Http.Error String)


hitUrl : String -> Cmd Msg
hitUrl url =
    let
        cmd =
            Http.send UrlMsg (buildRequest url)
    in
    cmd


buildRequest : String -> Http.Request String
buildRequest url =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse showResponse
        , timeout = Nothing
        , withCredentials = False
        }


showResponse : Http.Response String -> Result String String
showResponse resp =
    Just (toString resp)
        |> Result.fromMaybe "unknown"


changeUrl : Model -> String -> Model
changeUrl model newUrl =
    { model | url = newUrl }


decodeRawResponse : String -> DecodedResponse
decodeRawResponse raw =
    { headers = Dict.empty
    , contents = Nothing
    , status =
        { code = 0
        , message = ""
        }
    }


updateResponse : Model -> String -> Model
updateResponse model value =
    { model | response = Just { rawResponse = value, url = model.url, error = "", response = Just (decodeRawResponse value) } }


updateErrorResponse : Model -> String -> Model
updateErrorResponse model error =
    { model | response = Just { rawResponse = "", url = model.url, error = toString error, response = Nothing } }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUrl newUrl ->
            ( changeUrl model newUrl
            , Cmd.none
            )

        Submit ->
            ( model, hitUrl model.url )

        UrlMsg (Ok value) ->
            ( updateResponse model value, Cmd.none )

        UrlMsg (Err error) ->
            ( updateErrorResponse model (toString error), Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        responseMarkup =
            case model.response of
                Nothing ->
                    pre [] [ text "<>" ]

                Just response ->
                    div []
                        [ p [ class "Result__urlDisplay" ] [ text response.url ]
                        , pre []
                            [ code []
                                [ text
                                    response.rawResponse
                                ]
                            , h4 [] [ text response.error ]
                            ]
                        ]
    in
    div []
        [ Html.form [ class "UrlForm", onSubmit Submit, action "javascript:void(0)" ]
            [ input [ class "UrlForm__input", name "url", type_ "text", placeholder "Enter url here", onInput ChangeUrl, value model.url ] []
            , button [ class "UrlForm__button", type_ "Submit" ] [ text "Submit" ]
            ]
        , div [ class "Result" ] [ responseMarkup ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
