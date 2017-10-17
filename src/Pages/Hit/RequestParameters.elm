module Pages.Hit.RequestParameters
    exposing
        ( RequestParameters
        , empty
        , pushBlank
        , remove
        , updateName
        , updateValue
        , valid
        , view
        )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Msgs exposing (Msg)


-- TYPES --


type alias RequestParameter =
    { name : String
    , value : String
    }


type alias Position =
    Int


type alias RequestParameters =
    Dict Position RequestParameter



-- CONSTANTS --


blankRequestParameter : RequestParameter
blankRequestParameter =
    RequestParameter "" ""


empty : RequestParameters
empty =
    Dict.empty



-- METHODS --


pushBlank : RequestParameters -> RequestParameters
pushBlank requestParameters =
    push blankRequestParameter requestParameters


push : RequestParameter -> RequestParameters -> RequestParameters
push requestParameter requestParameters =
    Dict.insert (Dict.size requestParameters) requestParameter requestParameters


updateName : Position -> String -> RequestParameters -> RequestParameters
updateName position newName requestParameters =
    let
        requestParameter =
            Dict.get position requestParameters

        newRequestParameter =
            case requestParameter of
                Just requestParameter_ ->
                    { requestParameter_ | name = newName }

                Nothing ->
                    RequestParameter newName ""
    in
        updateRequestParameter position newRequestParameter requestParameters


updateValue : Position -> String -> RequestParameters -> RequestParameters
updateValue position newValue requestParameters =
    let
        requestParameter =
            Dict.get position requestParameters

        newRequestParameter =
            case requestParameter of
                Just requestParameter_ ->
                    { requestParameter_ | value = newValue }

                Nothing ->
                    RequestParameter newValue ""
    in
        updateRequestParameter position newRequestParameter requestParameters


updateRequestParameter : Position -> RequestParameter -> RequestParameters -> RequestParameters
updateRequestParameter position newRequestParameter requestParameters =
    Dict.update position (\_ -> Just newRequestParameter) requestParameters


remove : Position -> RequestParameters -> RequestParameters
remove position requestParameters =
    Dict.remove position requestParameters
        |> Dict.foldl
            (\_ requestParameter newRequestParameters ->
                Dict.insert (Dict.size newRequestParameters) requestParameter newRequestParameters
            )
            Dict.empty



-- VIEW --


viewRequestParameter : Position -> RequestParameter -> Html Msg
viewRequestParameter position requestParameter =
    div [ class "form-row" ]
        [ viewRequestParameterName position requestParameter
        , div [ class "col" ]
            [ input
                [ type_ "text"
                , placeholder "Enter Value"
                , class "input form-control api-req-form__input"
                , value requestParameter.value
                , onInput (Msgs.ChangeRequestParameterValue position)
                ]
                []
            ]
        , div [ class "col" ]
            [ a
                [ href "javascript:void(0)"
                , class "RequestParameters__delete"
                , onClick (Msgs.DeleteRequestParameter position)
                ]
                [ text "×" ]
            ]
        ]


viewRequestParameterName : Position -> RequestParameter -> Html Msg
viewRequestParameterName position { name } =
    let
        defaultClass =
            "input form-control api-req-form__input"

        updatedClass =
            if present name then
                defaultClass
            else
                defaultClass ++ " is-invalid"

        viewValidationError =
            if present name then
                text ""
            else
                div [ class "invalid-feedback" ]
                    [ text "Cannot be empty" ]
    in
        div [ class "col" ]
            [ input
                [ type_ "text"
                , placeholder "Enter Name"
                , class updatedClass
                , value name
                , onInput (Msgs.ChangeRequestParameterName position)
                ]
                []
            , viewValidationError
            ]


viewRequestParameters : RequestParameters -> Html Msg
viewRequestParameters requestParameters =
    div [ class "aapi-req-form__form-inline" ]
        (requestParameters
            |> Dict.map viewRequestParameter
            |> Dict.toList
            |> List.map (\( _, viewRequestParameter ) -> viewRequestParameter)
        )


view : RequestParameters -> Html Msg
view requestParameters =
    div [ class "form-group" ]
        [ div [ class "form-group__label" ]
            [ span [] [ text "Request Parameters" ]
            , a [ href "javascript:void(0)", class "devise-links", onClick Msgs.AddRequestParameter ] [ text "Add Parameter" ]
            ]
        , viewRequestParameters requestParameters
        ]



-- UTILITY FUNCTIONS


valid : RequestParameters -> Bool
valid requestParameters =
    requestParameters
        |> Dict.values
        |> List.map (\{ name } -> present name)
        |> List.member False
        |> not


present : String -> Bool
present value =
    value |> String.trim |> String.isEmpty |> not
