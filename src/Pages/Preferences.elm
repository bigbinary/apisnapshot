module Pages.Preferences exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Models exposing (Model)
import Msgs exposing (Msg)


view : Model -> Html Msg
view model =
    div [ class "col-md-12" ]
        [ p [] [ text "This is a preferences page" ]
        ]
