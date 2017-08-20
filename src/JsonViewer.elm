module JsonViewer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import JSVal


type alias ElementKey =
    String


type alias UniqueId =
    String


type alias JVCollection =
    -- This type can hold both objects and arrays. The `elementKey` of arrays are their positional indices.
    List ( UniqueId, ElementKey, JsonView )


type JsonView
    = JVString String
    | JVFloat Float
    | JVInt Int
    | JVBool Bool
    | JVNull
    | JVArray JVCollection
    | JVObject JVCollection


isJsonElement : JsonView -> Bool
isJsonElement jsonVal =
    case jsonVal of
        JVString _ ->
            True

        JVFloat _ ->
            True

        JVInt _ ->
            True

        JVNull ->
            True

        JVBool _ ->
            True

        _ ->
            False


isJsonCollection : JsonView -> Bool
isJsonCollection jsonVal =
    case jsonVal of
        JVArray _ ->
            True

        JVObject _ ->
            True

        _ ->
            False


getCollection : JsonView -> JVCollection
getCollection jsonVal =
    case jsonVal of
        JVArray collection ->
            collection

        JVObject collection ->
            collection

        _ ->
            []



---- Construct a JsonViewer from plain JSVal ----


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
