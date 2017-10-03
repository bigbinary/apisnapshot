module Msgs exposing (..)

import Http


type Msg
    = Submit
    | ChangeUrl String
    | ResponseAvailable (Result Http.Error (Http.Response String))
    | ToggleJsonCollectionView String
    | MoreActionsDropdownChange String
    | HttpMethodsDropdownChange String
    | AddRequestParameter
    | ChangeRequestParameterName Int String
    | ChangeRequestParameterValue Int String
    | DeleteRequestParameter Int
