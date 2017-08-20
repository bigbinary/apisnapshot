module Msg exposing (..)

import Http


type Msg
    = Submit
    | ChangeUrl String
    | ResponseAvailable (Result Http.Error (Http.Response String))
    | ToggleJsonCollectionView String
