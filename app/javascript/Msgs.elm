module Msgs exposing (..)

import Http
import Navigation exposing (Location)
import Response exposing (Response)
import RemoteData exposing (WebData)


type Msg
    = Submit
    | ChangeUrl String
    | OnSubmitResponse (WebData Response)
    | OnHitFetchResponse (WebData Response)
    | ToggleJsonCollectionView String
    | MoreActionsDropdownChange String
    | HttpMethodsDropdownChange String
    | AddRequestParameter
    | ChangeRequestParameterName Int String
    | ChangeRequestParameterValue Int String
    | DeleteRequestParameter Int
    | OnLocationChange Location
    | ShowRawResponse
    | ShowFormattedResponse
