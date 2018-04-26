module Views.NotFound exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : model -> Html msg
view _ =
    div [ class "col-md-12" ]
        [ p [] [ text "Not found" ]
        ]
