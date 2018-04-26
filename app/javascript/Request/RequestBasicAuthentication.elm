module Request.RequestBasicAuthentication exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode exposing (..)


type alias BasicAuthentication =
    { username : String
    , password : String
    }


empty : BasicAuthentication
empty =
    { username = ""
    , password = ""
    }



-- UPDATE --


type Msg
    = RemoveBasicAuthentication
    | UpdateUsername String
    | UpdatePassword String


update : Msg -> Maybe BasicAuthentication -> ( Maybe BasicAuthentication, Cmd Msg )
update msg model =
    case msg of
        UpdateUsername n ->
            let
                new =
                    Maybe.map (\m -> { m | username = n }) model
            in
                ( new, Cmd.none )

        UpdatePassword p ->
            let
                new =
                    Maybe.map (\m -> { m | password = p }) model
            in
                ( new, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW --


view : BasicAuthentication -> Html Msg
view b =
    div [ class "form-group" ]
        [ div [ class "form-group__label" ] [ span [] [ text "Basic Authentication" ] ]
        , div [ class "aapi-req-form__form-inline" ]
            [ div [ class "form-row" ]
                [ div [ class "col" ]
                    [ input
                        [ type_ "text"
                        , placeholder "Username"
                        , class "input form-control api-req-form__input"
                        , value b.username
                        , onInput (UpdateUsername)
                        ]
                        []
                    ]
                , div [ class "col" ]
                    [ input
                        [ type_ "password"
                        , placeholder "Password"
                        , class "input form-control api-req-form__input"
                        , value b.password
                        , onInput (UpdatePassword)
                        ]
                        []
                    ]
                , div [ class "col" ]
                    [ a
                        [ href "javascript:void(0)"
                        , class "RequestBasicAuthentication__delete"
                        , onClick (RemoveBasicAuthentication)
                        ]
                        [ text "×" ]
                    ]
                ]
            ]
        ]
