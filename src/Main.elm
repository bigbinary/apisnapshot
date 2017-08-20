module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import JSVal
import Json.Decode
import JsonViewer
import Msg exposing (Msg)
import Set


---- MODEL ----


type alias Response =
    { raw : Http.Response String
    , collapsedNodes : JsonViewer.CollapsedNodes
    , json : JsonViewer.JsonView
    }


type alias Model =
    { url : String, response : Maybe Response, error : Maybe Http.Error }


init : ( Model, Cmd Msg )
init =
    ( { url = "https://swapi.co/api/people/1/", response = Nothing, error = Nothing }, Cmd.none )



---- UPDATE ----


hitUrl : String -> Cmd Msg
hitUrl url =
    let
        cmd =
            Http.send Msg.ResponseAvailable (buildRequest url)
    in
    cmd


buildRequest : String -> Http.Request (Http.Response String)
buildRequest url =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse preserveFullResponse
        , timeout = Nothing
        , withCredentials = False
        }


preserveFullResponse : Http.Response String -> Result String (Http.Response String)
preserveFullResponse resp =
    Ok resp


parseResponseBodyToJSVal : Http.Response String -> JSVal.JSVal
parseResponseBodyToJSVal httpResponse =
    let
        result =
            Json.Decode.decodeString JSVal.decoder httpResponse.body
    in
    case result of
        Ok v ->
            v

        Err s ->
            JSVal.JSString ("Error parsing the body. " ++ s)


updateResponse : Model -> Http.Response String -> Model
updateResponse model httpResponse =
    { model
        | error = Nothing
        , response =
            Just
                { raw = httpResponse
                , collapsedNodes = Set.empty
                , json =
                    JsonViewer.fromJSVal (parseResponseBodyToJSVal httpResponse)
                }
    }


updateErrorResponse : Model -> Http.Error -> Model
updateErrorResponse model error =
    { model | error = Just error, response = Nothing }


changeUrl : Model -> String -> Model
changeUrl model newUrl =
    { model | url = newUrl }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.ChangeUrl newUrl ->
            ( changeUrl model newUrl
            , Cmd.none
            )

        Msg.Submit ->
            ( model, hitUrl model.url )

        Msg.ResponseAvailable (Ok value) ->
            ( updateResponse model value, Cmd.none )

        Msg.ResponseAvailable (Err error) ->
            ( updateErrorResponse model error, Cmd.none )

        Msg.ToggleJsonCollectionView id ->
            ( case model.response of
                Just response ->
                    let
                        collapsedNodes =
                            response.collapsedNodes
                    in
                    if Set.member id collapsedNodes then
                        { model | response = Just { response | collapsedNodes = Set.remove id collapsedNodes } }
                    else
                        { model | response = Just { response | collapsedNodes = Set.insert id collapsedNodes } }

                Nothing ->
                    model
            , Cmd.none
            )



---- VIEW ----


httpStatusMarkup : Http.Response String -> Html msg
httpStatusMarkup response =
    div []
        [ p [ class "Result__urlDisplay" ] [ text response.url ]
        , p [] [ text ("Status: " ++ toString response.status.code) ]
        , p [] [ text response.status.message ]
        ]


httpRawResponseMarkup : Http.Response String -> Html msg
httpRawResponseMarkup response =
    div []
        [ h3 [] [ text "Raw Response Body" ]
        , pre [] [ code [] [ text response.body ] ]
        ]


httpErrorResponseToMarkup : Http.Response String -> Html msg
httpErrorResponseToMarkup response =
    div []
        [ httpStatusMarkup response, httpRawResponseMarkup response ]


errorToMarkup : Http.Error -> Html msg
errorToMarkup error =
    case error of
        Http.BadUrl url ->
            p [ class "Error" ] [ text ("Bad Url! " ++ url) ]

        Http.Timeout ->
            p [ class "Error" ] [ text "Sorry the request timed out" ]

        Http.NetworkError ->
            p [ class "Error" ] [ text "There was a network error." ]

        Http.BadStatus response ->
            div [] [ p [ class "Error" ] [ text "Server returned an error." ], httpErrorResponseToMarkup response ]

        Http.BadPayload message response ->
            div [] [ p [ class "Error" ] [ text ("Bad payload error: " ++ message) ], httpErrorResponseToMarkup response ]


emptyResponseMarkup : Model -> Html msg
emptyResponseMarkup model =
    div []
        [ case model.error of
            Just error ->
                errorToMarkup error

            -- We haven't made any requests so far; no errors, no response, nothing.
            Nothing ->
                text ""
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        responseMarkup =
            case model.response of
                Nothing ->
                    [ emptyResponseMarkup model ]

                Just response ->
                    let
                        rootNode =
                            { jsonVal = response.json
                            , uniqueId = "root"
                            , depth = 0
                            , collapsedNodes = response.collapsedNodes
                            }
                    in
                    [ httpStatusMarkup response.raw
                    , div [ class "Result__jsonView" ] [ JsonViewer.view rootNode ]
                    , httpRawResponseMarkup response.raw
                    ]
    in
    div []
        [ Html.form [ class "UrlForm", onSubmit Msg.Submit, action "javascript:void(0)" ]
            [ input
                [ class "UrlForm__input"
                , name "url"
                , type_ "text"
                , placeholder "Enter url here"
                , onInput Msg.ChangeUrl
                , value model.url
                ]
                []
            , button [ class "UrlForm__button", type_ "Submit" ] [ text "Submit" ]
            ]
        , div [ class "Result" ] responseMarkup
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
