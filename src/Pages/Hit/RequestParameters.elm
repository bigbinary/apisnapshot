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

        newRequestParam =
            case requestParameter of
                Just requestParam_ ->
                    { requestParam_ | name = newName }

                Nothing ->
                    RequestParameter newName ""
    in
        updateParam position newRequestParam requestParameters


updateValue : Position -> String -> RequestParameters -> RequestParameters
updateValue position newValue requestParameters =
    let
        requestParameter =
            Dict.get position requestParameters

        newRequestParam =
            case requestParameter of
                Just requestParam_ ->
                    { requestParam_ | value = newValue }

                Nothing ->
                    RequestParameter newValue ""
    in
        updateParam position newRequestParam requestParameters


updateParam : Position -> RequestParameter -> RequestParameters -> RequestParameters
updateParam position newRequestParam requestParameters =
    Dict.update position (\_ -> Just newRequestParam) requestParameters


remove : Position -> RequestParameters -> RequestParameters
remove position requestParameters =
    Dict.remove position requestParameters
        |> Dict.foldl
            (\_ requestParameter newRequestParams ->
                Dict.insert (Dict.size newRequestParams) requestParameter newRequestParams
            )
            Dict.empty



-- VIEW --


viewRequestParameter : Position -> RequestParameter -> Html Msg
viewRequestParameter position requestParameter =
    div [ class "form-row" ]
        [ div [ class "col" ]
            [ input
                [ type_ "text"
                , placeholder "Enter Name"
                , class "input form-control api-req-form__input"
                , value requestParameter.name
                , onInput (Msgs.ChangeRequestParameterName position)
                ]
                []
            ]
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
