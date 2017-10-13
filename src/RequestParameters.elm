module RequestParameters
    exposing
        ( RequestParameter
        , RequestParameters
        , empty
        , pushBlank
        , updateName
        , updateValue
        , remove
        , view
        )

import Array
import Array.Extra
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Msgs exposing (Msg)


-- TYPES --


type alias RequestParameter =
    { name : String
    , value : String
    }


type alias RequestParameters =
    Array.Array RequestParameter



-- CONSTANTS --


blankRequestParameter : RequestParameter
blankRequestParameter =
    { name = ""
    , value = ""
    }


empty : RequestParameters
empty =
    Array.empty



-- METHODS --


pushBlank : RequestParameters -> RequestParameters
pushBlank requestParameters =
    push blankRequestParameter requestParameters


push : RequestParameter -> RequestParameters -> RequestParameters
push requestParameter requestParameters =
    Array.push requestParameter requestParameters


updateName : Int -> String -> RequestParameters -> RequestParameters
updateName index newName requestParameters =
    let
        item =
            Array.get index requestParameters

        updatedItem =
            case item of
                Just item_ ->
                    { item_ | name = newName }

                Nothing ->
                    blankRequestParameter

        updatedRequestedParameters =
            Array.set index updatedItem requestParameters
    in
        updatedRequestedParameters


updateValue : Int -> String -> RequestParameters -> RequestParameters
updateValue index newValue requestParameters =
    let
        item =
            Array.get index requestParameters

        updatedItem =
            case item of
                Just item_ ->
                    { item_ | value = newValue }

                Nothing ->
                    blankRequestParameter

        updatedRequestedParameters =
            Array.set index updatedItem requestParameters
    in
        updatedRequestedParameters


remove : Int -> RequestParameters -> RequestParameters
remove index requestParameters =
    Array.Extra.removeAt index requestParameters



-- VIEW --


itemView : Int -> RequestParameter -> Html Msg
itemView index requestParameter =
    li [ attribute "data-param-id" (toString index) ]
        [ input
            [ type_ "text"
            , placeholder "Enter Name"
            , value requestParameter.name
            , onInput (Msgs.ChangeRequestParameterName index)
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Enter Value"
            , value requestParameter.value
            , onInput (Msgs.ChangeRequestParameterValue index)
            ]
            []
        , a
            [ href "javascript:void(0)"
            , class "RequestParameters__delete"
            , onClick (Msgs.DeleteRequestParameter index)
            ]
            [ text "×" ]
        ]


view : RequestParameters -> Html Msg
view requestParameters =
    ul []
        (requestParameters
            |> Array.toIndexedList
            |> List.map (\( index, requestParameter ) -> itemView index requestParameter)
        )
