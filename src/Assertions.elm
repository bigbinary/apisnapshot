module Assertions exposing (..)

import Array
import Array.Extra
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Msgs exposing (Msg)


-- TYPES --


type State
    = EMPTY
    | PASSED
    | FAILED


type alias Assertion =
    { key : String
    , value : String
    , state : State
    , keyError : Maybe String
    , valueError : Maybe String
    }


type alias Assertions =
    Array.Array Assertion



-- CONSTANTS --


blankAssertion : Assertion
blankAssertion =
    { key = ""
    , value = ""
    , state = EMPTY
    , keyError = Nothing
    , valueError = Nothing
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
                    { item_ | key = newName }

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


classForFieldError error =
    case error of
        Nothing ->
            ""

        Just _ ->
            "fieldError"


textForState state =
    case state of
        EMPTY ->
            ""

        PASSED ->
            "Passed"

        FAILED ->
            "Failed"


itemView : Int -> Assertion -> Html Msg
itemView index assertion =
    li [ attribute "data-assertion-id" (toString index) ]
        [ input
            [ type_ "text"
            , placeholder "Enter key name"
            , value assertion.key
            , onInput (Msgs.ChangeAssertionName index)
            , class (classForFieldError assertion.keyError)
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Enter Value"
            , value assertion.value
            , onInput (Msgs.ChangeAssertionValue index)
            , class (classForFieldError assertion.valueError)
            ]
            []
        , a
            [ href "javascript:void(0)"
            , class "RequestParameters__delete"
            , onClick (Msgs.DeleteAssertion index)
            ]
            [ text "×" ]
        , span [] [ text (textForState assertion.state) ]
        ]


view : Assertions -> Html Msg
view assertions =
    ul []
        (assertions
            |> Array.toIndexedList
            |> List.map (\( index, assertion ) -> itemView index assertion)
        )
