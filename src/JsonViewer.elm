module JsonViewer exposing (CollapsedNodePaths, JsonView, fromJSVal, view, rootNodePath)

{-| JsonViewer transforms the parsed JSON data (`JSVal`) into the
renderable structure `JsonView`, and provides a renderer for it in
which the user can collapse and expand nested values.

While `JSVal` is a plain tree of data that comes from simply parsing the
incoming JSON response, the `JsonView` structure defined here adds a
unique path to every element, and also transforms both objects and arrays
into a homogeneous collection type so that they can be rendered with the
same code.

Having a unique path makes it possible to refer unambiguously to any
value in the JSON, which is needed since we don't have object references in
value-based programming. Such an unique path can be used for example,
to maintain a list of collapsed nodes.


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


{-| Set of absolute paths of all collapsed nodes
-}
type alias CollapsedNodePaths =
    Set.Set NodePath


{-| The key to render for each collection item.
Objects have their own keys; arrays use their index.
-}
type alias ElementKey =
    String


{-| Unique absolute path for every element in the JSON.

This is required so that we can record whether a specific node
is expanded or collapsed in the Collapsed set, and seek it out
when rendering the node.

This is guaranteed to be unique as it encodes the entire path
from the root to itself.

For example `root.2.name` points to the value whose
key is `name`, of the object that is the second element in the root array.

-}
type alias NodePath =
    String


rootNodePath : NodePath
rootNodePath =
    "root"


type alias JVCollectionElement =
    -- A collection element could belong to either an Object or an Array.
    -- For objects, their `ElementKey` is the key itself, and for arrays it is the element index.
    ( NodePath, ElementKey, JsonView )


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
    , collapsedNodePaths : CollapsedNodePaths
    , nodePath : NodePath
    , jsonVal : JsonView
    }



---- Construct a JsonView from plain JSVal ----


mapArrayElements : List JSVal.JSVal -> NodePath -> List JVCollectionElement
mapArrayElements jsValsList parentNodePath =
    List.indexedMap
        (\id jsValElement ->
            let
                nodePath =
                    parentNodePath ++ "." ++ toString id
            in
            ( nodePath, toString id, fromJSVal_ jsValElement nodePath )
        )
        jsValsList


mapObjectElements : List ( ElementKey, JSVal.JSVal ) -> NodePath -> List JVCollectionElement
mapObjectElements jsValsList parentNodePath =
    List.map
        (\( key, jsVal ) ->
            let
                nodePath =
                    parentNodePath ++ "." ++ key
            in
            ( nodePath, key, fromJSVal_ jsVal nodePath )
        )
        jsValsList


fromJSVal_ : JSVal.JSVal -> NodePath -> JsonView
fromJSVal_ jsVal parentNodePath =
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
            JVArray (mapArrayElements array parentNodePath)

        JSVal.JSObject object ->
            JVObject (mapObjectElements object parentNodePath)


fromJSVal : JSVal.JSVal -> JsonView
fromJSVal jsVal =
    fromJSVal_ jsVal rootNodePath



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
        { jsonVal, collapsedNodePaths, nodePath } =
            node

        isCollapsed =
            Set.member nodePath collapsedNodePaths

        render collection caption =
            span
                [ class "JsonView__collapsible"
                , onClick (Msg.ToggleJsonCollectionView nodePath)
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
collectionItemView parentNode ( nodePath, elementKey, jsonVal ) =
    let
        node =
            { parentNode | jsonVal = jsonVal, nodePath = nodePath }
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
        { collapsedNodePaths, depth, nodePath } =
            parentNode

        isCollapsed =
            Set.member nodePath collapsedNodePaths
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
