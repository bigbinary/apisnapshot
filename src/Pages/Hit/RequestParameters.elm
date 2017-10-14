module Pages.Hit.RequestParameters
    exposing
        ( RequestParameters
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
    div [ class "api-req-form__form-inline form-inline", attribute "data-param-id" (toString index) ]
        [ input
            [ type_ "text"
            , placeholder "Enter Name"
            , class "input form-control api-req-form__input"
            , value requestParameter.name
            , onInput (Msgs.ChangeRequestParameterName index)
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Enter Value"
            , class "input form-control api-req-form__input"
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


fields : RequestParameters -> Html Msg
fields requestParameters =
    ul []
        (requestParameters
            |> Array.toIndexedList
            |> List.map (\( index, requestParameter ) -> itemView index requestParameter)
        )


view : RequestParameters -> Html Msg
view requestParameters =
    div []
        [ div [ class "form-group__label" ]
            [ span [] [ text "Request Parameters" ]
            , a [ href "javascript:void(0)", class "devise-links", onClick Msgs.AddRequestParameter ]
                [ text "Add Parameter" ]
            ]
        , fields requestParameters
        ]
