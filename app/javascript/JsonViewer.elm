module JsonViewer exposing (fromJSVal, view, rootNodePath)

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
import JsonViewerTypes exposing (..)
import JSVal
import Msgs exposing (Msg)
import Set


-- Indentation in pixels when rendering a nested structure


indent : Int
indent =
    16


arrowRight : Html msg
arrowRight =
    span [ class "JsonView__collapseArrow" ] [ text "▶ " ]


arrowDown : Html msg
arrowDown =
    span [ class "JsonView__collapseArrow" ] [ text "▼ " ]


rootNodePath : NodePath
rootNodePath =
    "root"


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
        (\index jsValElement ->
            let
                nodePath =
                    parentNodePath ++ "." ++ toString index
            in
                ( nodePath, toString index, fromJSVal_ jsValElement nodePath )
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


collectionItemPrefix : Node -> Html Msg
collectionItemPrefix node =
    let
        { jsonVal, collapsedNodePaths, nodePath } =
            node

        isCollapsed =
            Set.member nodePath collapsedNodePaths

        render collection caption =
            span
                [ class "JsonView__collapsible"
                , onClick (Msgs.ToggleJsonCollectionView nodePath)
                ]
                [ if isCollapsed then
                    arrowRight
                  else
                    arrowDown
                , text caption
                ]
    in
        case jsonVal of
            JVArray collection ->
                render collection "["

            JVObject collection ->
                render collection "{"

            _ ->
                Html.text ""


collectionItemPostfix : Node -> Html Msg
collectionItemPostfix { jsonVal } =
    case jsonVal of
        JVArray collection ->
            text "],"

        JVObject collection ->
            text "},"

        _ ->
            text ","


collectionItemView : Bool -> Node -> JVCollectionElement -> Html Msg
collectionItemView showPropertyKey parentNode ( nodePath, elementKey, jsonVal ) =
    let
        node =
            { parentNode | jsonVal = jsonVal, nodePath = nodePath }

        propertyKey =
            if showPropertyKey then
                span
                    [ class "JsonView__propertyKey" ]
                    [ text (elementKey ++ ":") ]
            else
                text ""
    in
        li [ class "JsonView__collectionItem" ]
            [ propertyKey
            , collectionItemPrefix node
            , view node
            , collectionItemPostfix node
            ]


collectionView : Node -> JVCollection -> Bool -> Html Msg
collectionView parentNode collection showPropertyKey =
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
                    (collectionItemView showPropertyKey { parentNode | depth = depth + 1 })
                    collection
                )


view : Node -> Html Msg
view node =
    span [ class "json-view" ] [ (view2 node) ]


view2 : Node -> Html Msg
view2 node =
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
            span [ class "JsonView__null" ] [ text "null" ]

        JVArray array ->
            let
                rendered =
                    collectionView node array False
            in
                if node.depth == 0 then
                    li [ class "JsonView__collectionItem" ]
                        [ collectionItemPrefix node
                        , rendered
                        , collectionItemPostfix node
                        ]
                else
                    rendered

        JVObject object ->
            let
                rendered =
                    collectionView node object True
            in
                if node.depth == 0 then
                    li [ class "JsonView__collectionItem" ]
                        [ collectionItemPrefix node
                        , rendered
                        , collectionItemPostfix node
                        ]
                else
                    rendered
