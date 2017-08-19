module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


---- MODEL ----


type alias BBJson =
    { name : String }


type alias Response =
    { error : String
    , raw : String
    , json : Maybe BBJson
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUrl newUrl ->
            let
                newModel =
                    { model | url = newUrl }
            in
            ( newModel, Cmd.none )

        Submit ->
            ( model, hitUrl model.url )

        UrlMsg (Ok value) ->
            let
                newModel =
                    { model | response = Just { raw = value, error = "", json = Nothing } }
            in
            ( newModel, Cmd.none )

        UrlMsg (Err error) ->
            let
                newModel =
                    { model | response = Just { raw = "", error = toString error, json = Nothing } }
            in
            ( newModel, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        responseMarkup =
            case model.response of
                Nothing ->
                    pre [] [ text "<>" ]

                Just response ->
                    pre []
                        [ code []
                            [ text
                                response.raw
                            ]
                        , h4 [] [ text response.error ]
                        ]
    in
    div []
        [ Html.form [ class "UrlForm", onSubmit Submit, action "javascript:void(0)" ]
            [ input [ class "UrlForm__input", name "url", type_ "text", placeholder "Enter url here", onInput ChangeUrl, value model.url ] []
            , button [ class "UrlForm__button", type_ "Submit" ] [ text "Submit" ]
            ]
        , div [ class "Result" ]
            [ p [ class "Result__urlDisplay" ] [ text model.url ]
            , responseMarkup
            ]
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
