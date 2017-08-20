module JsonViewer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import JSVal


type alias ElementIndex =
    String


type alias ObjectKey =
    String


type JsonView
    = JVString String
    | JVFloat Float
    | JVInt Int
    | JVBool Bool
    | JVNull
    | JVArray (List ( ElementIndex, JsonView ))
    | JVObject (List ( ObjectKey, JsonView ))



---- Construct a JsonViewer from plain JSVal ----


mapArrayElements : List JSVal.JSVal -> String -> List ( ElementIndex, JsonView )
mapArrayElements jsValsList parentId =
    List.indexedMap
        (\id jsValElement ->
            let
                elementId =
                    parentId ++ "-" ++ toString id
            in
            ( elementId, fromJSVal_ jsValElement elementId )
        )
        jsValsList


mapObjectElements : List ( ObjectKey, JSVal.JSVal ) -> String -> List ( ObjectKey, JsonView )
mapObjectElements jsValsList parentId =
    List.map
        (\( key, jsVal ) ->
            let
                elementId =
                    parentId ++ "-" ++ key
            in
            ( key, fromJSVal_ jsVal elementId )
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
