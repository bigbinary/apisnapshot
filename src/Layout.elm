module Layout exposing (embed)

import Html exposing (Html, div, ul, li, a, text)
import Html.Attributes exposing (class, href)


embed : model -> Html msg -> Html msg
embed _ view =
    div []
        [ nav
        , div [ class "container-fluid api-req-form__container" ] [ view ]
        ]


nav : Html msg
nav =
    div []
        [ a [ href "#preferences" ] [ text "Preferences" ] ]
