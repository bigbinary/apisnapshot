module JsonViewer exposing (CollapsedNodes, JsonView, fromJSVal, view)

{-| JsonViewer transforms the parsed JSON data (`JSVal`) into the
renderable structure `JsonView`, and provides a renderer for it in
which the user can collapse and expand nested values.

While `JSVal` is a plain tree of data that comes from simply parsing the
incoming JSON response, the `JsonView` structure defined here adds a
unique path to every element, and also transforms both objects and arrays
into a homogeneous collection type so that they can be rendered with the
same code.

Having a uniqueId makes it possible to refer unambiguously to any value
in the Json, which is needed since we don't have object references in
value-based programming.


# Usage

    let
        jsonView = (JsonViewer.fromJSVal (JSVal.String "Hello")
    in
        div [][JsonViewer.view jsonView)]

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import JSVal
import Msg
import Set


-- Indentation in pixels when rendering a nested structure


indent : Int
indent =
    16


arrowRight : String
arrowRight =
    "▶ "


arrowDown : String
arrowDown =
    "▼ "


{-| Set of all collapsed nodes
-}
type alias CollapsedNodes =
    Set.Set String


{-| The key to render for each collection item.
Objects have their own keys; arrays use their index.
-}
type alias ElementKey =
    String


{-| Unique string ID for every element in the JSON.

This is required so that we can record whether a specific node
is expanded or collapsed in the Collapsed set, and seek it out
when rendering the node.

This key is guaranteed to be unique as it encodes the entire path
from the root to itself. For example `root-2-name` points to the value whose
key is `name`, of the object that is the second element in the root array.

-}
type alias UniqueId =
    String


type alias JVCollectionElement =
    -- A collection element could belong to either an Object or an Array.
    -- For objects, their `ElementKey` is the key itself, and for arrays it is the element index.
    ( UniqueId, ElementKey, JsonView )


type alias JVCollection =
    List JVCollectionElement


type JsonView
    = JVString String
    | JVFloat Float
    | JVInt Int
    | JVBool Bool
    | JVNull
    | JVArray JVCollection
    | JVObject JVCollection


type alias Node =
    { depth : Int
    , collapsedNodes : CollapsedNodes
    , uniqueId : UniqueId
    , jsonVal : JsonView
    }



---- Construct a JsonView from plain JSVal ----


mapArrayElements : List JSVal.JSVal -> String -> List JVCollectionElement
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


mapObjectElements : List ( ElementKey, JSVal.JSVal ) -> String -> List JVCollectionElement
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


{-| Render the first line of a collection.

It looks like so:

    ▶ Object (3)
    ▼ Array (4)

Clicking on the arrow expands/collapses the collection.

Does not render anything for non-collections.

-}
firstSummaryLine node =
    let
        { jsonVal, collapsedNodes, uniqueId } =
            node

        isCollapsed =
            Set.member uniqueId collapsedNodes

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


collectionItemView : Node -> JVCollectionElement -> Html Msg.Msg
collectionItemView parentNode ( uniqueId, elementKey, jsonVal ) =
    let
        node =
            { parentNode | jsonVal = jsonVal, uniqueId = uniqueId }
    in
    li [ class "JsonView__collectionItem" ]
        [ span
            [ class "JsonView__propertyKey" ]
            [ text (elementKey ++ ":") ]
        , firstSummaryLine node
        , view node
        ]


collectionView : Node -> JVCollection -> String -> Html Msg.Msg
collectionView parentNode collection caption =
    let
        { collapsedNodes, depth, uniqueId } =
            parentNode

        isCollapsed =
            Set.member uniqueId collapsedNodes
    in
    if isCollapsed then
        Html.text ""
    else
        ol
            [ class "JsonView__collectionItemsList"
            , style [ ( "paddingLeft", toString ((depth + 1) * indent) ++ "px" ) ]
            ]
            (List.map
                (collectionItemView { parentNode | depth = depth + 1 })
                collection
            )


view : Node -> Html Msg.Msg
view node =
    case node.jsonVal of
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
                    collectionView node array "Array"
            in
            if node.depth == 0 then
                li [ class "JsonView__collectionItem" ]
                    [ firstSummaryLine node
                    , rendered
                    ]
            else
                rendered

        JVObject object ->
            let
                rendered =
                    collectionView node object "Object"
            in
            if node.depth == 0 then
                li [ class "JsonView__collectionItem" ]
                    [ firstSummaryLine node
                    , rendered
                    ]
            else
                rendered
