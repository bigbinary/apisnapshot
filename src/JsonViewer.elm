module JsonViewer exposing (Collapsed, JsonView, fromJSVal, toHtml)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import JSVal
import Msg
import Set


indent =
    16


arrowRight =
    "▶ "


arrowDown =
    "▼ "


type alias Collapsed =
    Set.Set String


type alias ElementKey =
    String


type alias UniqueId =
    String


type alias JVCollectionElement =
    ( UniqueId, ElementKey, JsonView )


type alias JVCollection =
    -- This type can hold both objects and arrays. The `elementKey` of arrays are their positional indices.
    List JVCollectionElement


type JsonView
    = JVString String
    | JVFloat Float
    | JVInt Int
    | JVBool Bool
    | JVNull
    | JVArray JVCollection
    | JVObject JVCollection



---- Construct a JsonView from plain JSVal ----


mapArrayElements : List JSVal.JSVal -> String -> List ( UniqueId, ElementKey, JsonView )
mapArrayElements jsValsList parentId =
    List.indexedMap
        (\id jsValElement ->
            let
                uniqueId =
                    parentId ++ "-" ++ toString id
            in
            ( uniqueId, toString id, fromJSVal_ jsValElement uniqueId )
        )
        jsValsList


mapObjectElements : List ( ElementKey, JSVal.JSVal ) -> String -> List ( UniqueId, ElementKey, JsonView )
mapObjectElements jsValsList parentId =
    List.map
        (\( key, jsVal ) ->
            let
                uniqueId =
                    parentId ++ "-" ++ key
            in
            ( uniqueId, key, fromJSVal_ jsVal uniqueId )
        )
        jsValsList


fromJSVal_ : JSVal.JSVal -> String -> JsonView
fromJSVal_ jsVal parentId =
    case jsVal of
        JSVal.JSString string ->
            JVString string

        JSVal.JSFloat float ->
            JVFloat float

        JSVal.JSInt int ->
            JVInt int

        JSVal.JSBool bool ->
            JVBool bool

        JSVal.JSNull ->
            JVNull

        JSVal.JSArray array ->
            JVArray (mapArrayElements array parentId)

        JSVal.JSObject object ->
            JVObject (mapObjectElements object parentId)


fromJSVal : JSVal.JSVal -> JsonView
fromJSVal jsVal =
    fromJSVal_ jsVal "root"



---- VIEW ----


firstSummaryLine jsonVal uniqueId collapsed =
    let
        isCollapsed =
            Set.member uniqueId collapsed

        render collection caption =
            span
                [ class "JsonView__collapsible"
                , onClick (Msg.ToggleJsonCollectionView uniqueId)
                ]
                [ if isCollapsed then
                    text arrowRight
                  else
                    text arrowDown
                , text (caption ++ " (" ++ toString (List.length collection) ++ ")")
                ]
    in
    case jsonVal of
        JVArray collection ->
            render collection "Array"

        JVObject collection ->
            render collection "Object"

        _ ->
            Html.text ""


jsonViewCollectionElementToHtml : Int -> Collapsed -> JVCollectionElement -> Html Msg.Msg
jsonViewCollectionElementToHtml depth collapsed ( uniqueId, elementKey, jsonVal ) =
    li [ class "JsonView__collectionItem" ]
        [ span
            [ class "JsonView__propertyKey" ]
            [ text (elementKey ++ ":") ]
        , firstSummaryLine jsonVal uniqueId collapsed
        , toHtml jsonVal uniqueId depth collapsed
        ]


jsonViewCollectionToHtml : JVCollection -> String -> UniqueId -> Int -> Collapsed -> Html Msg.Msg
jsonViewCollectionToHtml collection caption uniqueId depth collapsed =
    let
        isCollapsed =
            Set.member uniqueId collapsed
    in
    if isCollapsed then
        Html.text ""
    else
        ol [ class "JsonView__collectionItemsList", style [ ( "paddingLeft", toString ((depth + 1) * indent) ++ "px" ) ] ]
            (List.map
                (jsonViewCollectionElementToHtml (depth + 1) collapsed)
                collection
            )


toHtml : JsonView -> String -> Int -> Collapsed -> Html Msg.Msg
toHtml jsonVal id depth collapsed =
    case jsonVal of
        JVString string ->
            span [ class "JsonView__string" ] [ text string ]

        JVFloat float ->
            span [ class "JsonView__number" ] [ text (toString float) ]

        JVInt int ->
            span [ class "JsonView__number" ] [ text (toString int) ]

        JVBool bool ->
            span [ class "JsonView__bool" ] [ text (toString bool) ]

        JVNull ->
            span [ class "JsonView__null" ] [ text "(null)" ]

        JVArray array ->
            let
                rendered =
                    jsonViewCollectionToHtml array "Array" id depth collapsed
            in
            if depth == 0 then
                li [ class "JsonView__collectionItem" ] [ firstSummaryLine jsonVal id collapsed, rendered ]
            else
                rendered

        JVObject object ->
            let
                rendered =
                    jsonViewCollectionToHtml object "Object" id depth collapsed
            in
            if depth == 0 then
                li [ class "JsonView__collectionItem" ] [ firstSummaryLine jsonVal id collapsed, rendered ]
            else
                rendered
