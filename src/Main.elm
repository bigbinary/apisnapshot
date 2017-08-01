module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


---- MODEL ----


type alias Model =
    { url : String }


init : ( Model, Cmd Msg )
init =
    ( { url = "" }, Cmd.none )



---- UPDATE ----


type Msg
    = Submit
    | ChangeUrl String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUrl newUrl ->
            ( { model | url = newUrl }, Cmd.none )

        Submit ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Submit, action "javascript:void(0)" ]
            [ input [ name "url", type_ "text", placeholder "Enter url here", onInput ChangeUrl, value model.url ] []
            , button [ type_ "Submit" ] [ text "Submit" ]
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
