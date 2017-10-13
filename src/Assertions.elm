module Assertions
    exposing
        ( Assertions
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


type State
    = Empty
    | PASSED
    | FAILED


type alias Assertion =
    { name : String
    , value : String
    , state : State
    }


type alias Assertions =
    Array.Array Assertion



-- CONSTANTS --


blankAssertion : Assertion
blankAssertion =
    { name = ""
    , value = ""
    , state = Empty
    }


empty : Assertions
empty =
    Array.empty



-- METHODS --


pushBlank : Assertions -> Assertions
pushBlank assertions =
    push blankAssertion assertions


push : Assertion -> Assertions -> Assertions
push assertion assertions =
    Array.push assertion assertions


updateName : Int -> String -> Assertions -> Assertions
updateName index newName assertions =
    let
        item =
            Array.get index assertions

        updatedItem =
            case item of
                Just item_ ->
                    { item_ | name = newName }

                Nothing ->
                    blankAssertion

        updatedAssertions =
            Array.set index updatedItem assertions
    in
        updatedAssertions


updateValue : Int -> String -> Assertions -> Assertions
updateValue index newValue assertions =
    let
        item =
            Array.get index assertions

        updatedItem =
            case item of
                Just item_ ->
                    { item_ | value = newValue }

                Nothing ->
                    blankAssertion

        updatedAssertions =
            Array.set index updatedItem assertions
    in
        updatedAssertions


remove : Int -> Assertions -> Assertions
remove index assertions =
    Array.Extra.removeAt index assertions



-- VIEW --


itemView : Int -> Assertion -> Html Msg
itemView index assertion =
    li [ attribute "data-assertion-id" (toString index) ]
        [ input
            [ type_ "text"
            , placeholder "Enter Name"
            , value assertion.name
            , onInput (Msgs.ChangeAssertionName index)
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Enter Value"
            , value assertion.value
            , onInput (Msgs.ChangeAssertionValue index)
            ]
            []
        , a
            [ href "javascript:void(0)"
            , class "RequestParameters__delete"
            , onClick (Msgs.DeleteAssertion index)
            ]
            [ text "×" ]
        ]


view : Assertions -> Html Msg
view assertions =
    ul []
        (assertions
            |> Array.toIndexedList
            |> List.map (\( index, assertion ) -> itemView index assertion)
        )
