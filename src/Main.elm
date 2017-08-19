module Main exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD


---- MODEL ----


type alias BBJson =
    { name : String }


type alias Response =
    { original : Http.Response String
    , json : BBJson
    }


type alias Model =
    { url : String, response : Maybe Response, error : Maybe String }


init : ( Model, Cmd Msg )
init =
    ( { url = "https://swapi.co/api/people/1/", response = Nothing, error = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = Submit
    | ChangeUrl String
    | ResponseAvailable (Result Http.Error (Http.Response String))


hitUrl : String -> Cmd Msg
hitUrl url =
    let
        cmd =
            Http.send ResponseAvailable (buildRequest url)
    in
    cmd


buildRequest : String -> Http.Request (Http.Response String)
buildRequest url =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse parseResponse
        , timeout = Nothing
        , withCredentials = False
        }


parseResponse : Http.Response String -> Result String (Http.Response String)
parseResponse resp =
    Ok resp


changeUrl : Model -> String -> Model
changeUrl model newUrl =
    { model | url = newUrl }


updateResponse : Model -> Http.Response String -> Model
updateResponse model httpResponse =
    { model | error = Nothing, response = Just { original = httpResponse, json = { name = "Parsed" } } }


updateErrorResponse : Model -> String -> Model
updateErrorResponse model error =
    { model | error = Just (toString error), response = Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUrl newUrl ->
            ( changeUrl model newUrl
            , Cmd.none
            )

        Submit ->
            ( model, hitUrl model.url )

        ResponseAvailable (Ok value) ->
            ( updateResponse model value, Cmd.none )

        ResponseAvailable (Err error) ->
            ( updateErrorResponse model (toString error), Cmd.none )



---- VIEW ----


emptyResponseMarkup : Model -> Html msg
emptyResponseMarkup model =
    div []
        [ pre [] [ text "<>" ]
        , case model.error of
            Just message ->
                h4 [] [ text message ]

            Nothing ->
                text ""
        ]


successfulResponseMarkup : Response -> Html msg
successfulResponseMarkup response =
    div []
        [ p [ class "Result__urlDisplay" ] [ text (toString response.original.url) ]
        , pre [] [ code [] [ text response.original.body ] ]
        , p [] [ text ("Status code: " ++ toString response.original.status.code) ]
        , p [] [ text ("Status message: " ++ toString response.original.status.message) ]
        ]


view : Model -> Html Msg
view model =
    let
        responseMarkup =
            case model.response of
                Nothing ->
                    emptyResponseMarkup model

                Just response ->
                    successfulResponseMarkup response
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
