module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import JSVal
import Json.Decode
import JsonViewer
import Set


indent =
    12


arrowRight =
    "▶ "


arrowUp =
    "▲ "



---- MODEL ----


type alias Collapsed =
    Set.Set String


type alias Response =
    { original : Http.Response String
    , collapsed : Collapsed
    , json : JsonViewer.JsonView
    }


type alias Model =
    { url : String, response : Maybe Response, error : Maybe Http.Error }


init : ( Model, Cmd Msg )
init =
    ( { url = "https://swapi.co/api/people/1/", response = Nothing, error = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = Submit
    | ChangeUrl String
    | ResponseAvailable (Result Http.Error (Http.Response String))
    | ToggleJsonCollectionView String


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
        a =
            Json.Decode.decodeString JSVal.decoder httpResponse.body

        b =
            case a of
                Ok v ->
                    v

                Err s ->
                    JSVal.JSString ("Error parsing the body. " ++ s)
    in
    Debug.log (toString (JsonViewer.fromJSVal b))
        b


updateResponse : Model -> Http.Response String -> Model
updateResponse model httpResponse =
    { model
        | error = Nothing
        , response =
            Just
                { original = httpResponse
                , collapsed = Set.empty
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
        ChangeUrl newUrl ->
            ( changeUrl model newUrl
            , Cmd.none
            )

        Submit ->
            ( model, hitUrl model.url )

        ResponseAvailable (Ok value) ->
            ( updateResponse model value, Cmd.none )

        ResponseAvailable (Err error) ->
            ( updateErrorResponse model error, Cmd.none )

        ToggleJsonCollectionView id ->
            ( case model.response of
                Just response ->
                    let
                        collapsed =
                            response.collapsed
                    in
                    if Set.member id collapsed then
                        { model | response = Just { response | collapsed = Set.remove id collapsed } }
                    else
                        { model | response = Just { response | collapsed = Set.insert id collapsed } }

                Nothing ->
                    model
            , Cmd.none
            )



---- VIEW ----


httpResponseToMarkup : Http.Response String -> Html msg
httpResponseToMarkup response =
    div []
        [ p [ class "Result__urlDisplay" ] [ text (toString response.url) ]
        , pre [] [ code [] [ text response.body ] ]
        , p [] [ text ("Status code: " ++ toString response.status.code) ]
        , p [] [ text ("Status message: " ++ toString response.status.message) ]
        ]


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
            div [] [ p [ class "Error" ] [ text "Server complained of something..." ], httpResponseToMarkup response ]

        Http.BadPayload message response ->
            div [] [ p [ class "Error" ] [ text ("Server complained of bad payload: " ++ message) ], httpResponseToMarkup response ]


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


jsonViewToHtml : JsonViewer.JsonView -> String -> Int -> Collapsed -> Html Msg
jsonViewToHtml jsonView id depth collapsed =
    case jsonView of
        JsonViewer.JVString string ->
            span [ class "JsonView__string" ] [ text string ]

        JsonViewer.JVFloat float ->
            span [ class "JsonView__number" ] [ text (toString float) ]

        JsonViewer.JVInt int ->
            span [ class "JsonView__number" ] [ text (toString int) ]

        JsonViewer.JVBool bool ->
            span [ class "JsonView__bool" ] [ text (toString bool) ]

        JsonViewer.JVNull ->
            span [ class "JsonView__null" ] [ text "(null)" ]

        JsonViewer.JVArray array ->
            let
                isCollapsed =
                    Set.member id collapsed

                view =
                    [ span
                        [ class "JsonView__collapsible"
                        , onClick (ToggleJsonCollectionView id)
                        ]
                        [ if isCollapsed then
                            text arrowRight
                          else
                            text arrowUp
                        , text ("Array  (" ++ toString (List.length array) ++ ")")
                        ]
                    ]
            in
            if isCollapsed then
                div [] view
            else
                div [ style [ ( "marginLeft", toString (depth * indent) ++ "px" ) ] ]
                    (List.append
                        view
                        (List.indexedMap
                            (\index ( elementId, jsonVal ) ->
                                p []
                                    [ span
                                        [ class "JsonView__propertyKey" ]
                                        [ text (toString index ++ ": ") ]
                                    , jsonViewToHtml jsonVal (id ++ "--" ++ toString elementId) (depth + 1) collapsed
                                    ]
                            )
                            array
                        )
                    )

        JsonViewer.JVObject object ->
            let
                isCollapsed =
                    Set.member id collapsed

                view =
                    [ span
                        [ class "JsonView__collapsible"
                        , onClick (ToggleJsonCollectionView id)
                        ]
                        [ if isCollapsed then
                            text arrowRight
                          else
                            text arrowUp
                        , text ("Object (" ++ toString (List.length object) ++ ")")
                        ]
                    ]
            in
            if isCollapsed then
                div [] view
            else
                div [ style [ ( "marginLeft", toString (depth * indent) ++ "px" ) ] ]
                    (List.append
                        view
                        (List.map
                            (\( key, jsonVal ) ->
                                p []
                                    [ span [ class "JsonView__propertyKey" ] [ text (key ++ ": ") ]
                                    , jsonViewToHtml jsonVal (id ++ "--" ++ key) (depth + 1) collapsed
                                    ]
                            )
                            object
                        )
                    )


view : Model -> Html Msg
view model =
    let
        responseMarkup =
            case model.response of
                Nothing ->
                    [ emptyResponseMarkup model ]

                Just response ->
                    [ httpResponseToMarkup response.original
                    , div [ class "Result__jsonView" ] [ jsonViewToHtml response.json "root" 0 response.collapsed ]
                    ]
    in
    div []
        [ Html.form [ class "UrlForm", onSubmit Submit, action "javascript:void(0)" ]
            [ input [ class "UrlForm__input", name "url", type_ "text", placeholder "Enter url here", onInput ChangeUrl, value model.url ] []
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
