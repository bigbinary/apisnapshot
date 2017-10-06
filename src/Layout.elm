module Layout exposing (embed)

import Html exposing (Html, div, ul, li, a, text)
import Html.Attributes exposing (class, href)


embed : model -> Html msg -> Html msg
embed _ view =
    div []
        [ nav
        , div [ class "container-fluid page" ]
            [ div [ class "row" ] [ view ]
            ]
        ]


nav : Html msg
nav =
    div [ class "navbar navbar-default navbar-static-top" ]
        [ div [ class "container-fluid" ]
            [ div [ class "navbar-header" ]
                [ a
                    [ class "navbar-brand"
                    , href "#"
                    ]
                    [ text "ApiSanity" ]
                ]
            , div [ class "navbar-collapse collapse" ]
                [ ul [ class "nav navbar-nav navbar-right" ]
                    [ li []
                        [ a [ href "#preferences" ]
                            [ text "Preferences" ]
                        ]
                    ]
                ]
            ]
        ]
