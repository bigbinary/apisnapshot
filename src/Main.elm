module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JP


---- MODEL ----


type alias Model =
    { url : String, error : String, responseData : String }


init : ( Model, Cmd Msg )
init =
    ( { url = "", error = "", responseData = "" }, Cmd.none )


type alias Response =
    { status : Int
    }


responseDecoder : JD.Decoder Response
responseDecoder =
    JP.decode Response
        |> JP.required "status" JD.int


decodedValue : String -> String
decodedValue json =
    let
        result =
            JD.decodeString (JD.at [ "status" ] responseDecoder) json
    in
    case result of
        Ok value ->
            Debug.log "success"
                toString
                value

        Err error ->
            Debug.log "error"
                error



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
                    { model | url = newUrl, error = "", responseData = "" }
            in
            ( newModel, Cmd.none )

        Submit ->
            ( model, hitUrl model.url )

        UrlMsg (Ok value) ->
            let
                _ =
                    decodedValue value

                newModel =
                    { model | responseData = value }
            in
            ( newModel, Cmd.none )

        UrlMsg (Err error) ->
            let
                newModel =
                    { model | error = toString error }
            in
            ( newModel, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Submit, action "javascript:void(0)" ]
            [ input [ name "url", type_ "text", placeholder "Enter url here", onInput ChangeUrl, value model.url ] []
            , button [ type_ "Submit" ] [ text "Submit" ]
            ]
        , h2 [] [ text model.url ]
        , h2 [] [ text model.responseData ]
        , h4 [] [ text model.error ]
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
