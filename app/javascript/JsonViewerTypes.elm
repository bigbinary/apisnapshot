module JsonViewerTypes exposing (..)

import Set


{-| Set of absolute paths of all collapsed nodes
-}
type alias CollapsedNodePaths =
    Set.Set NodePath


type JsonView
    = JVString String
    | JVFloat Float
    | JVInt Int
    | JVBool Bool
    | JVNull
    | JVArray JVCollection
    | JVObject JVCollection


type alias JVCollection =
    List JVCollectionElement


type alias JVCollectionElement =
    -- A collection element could belong to either an Object or an Array.
    -- For objects, their `ElementKey` is the key itself, and for arrays it is the element index.
    ( NodePath, ElementKey, JsonView )


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
