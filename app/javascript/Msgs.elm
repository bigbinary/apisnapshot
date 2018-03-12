module Msgs exposing (..)

import Navigation exposing (Location)
import Response exposing (Response)
import Util exposing (DropDownAction)
import RemoteData exposing (WebData)
import Util exposing (RequestBody)


type Msg
    = Submit
    | ChangeUrl String
    | OnSubmitResponse (WebData Response)
    | OnHitFetchResponse (WebData Response)
    | ToggleJsonCollectionView String
    | MoreActionsDropdownChange DropDownAction
    | HttpMethodsDropdownChange String
    | AddRequestParameter
    | ChangeRequestParameterName Int String
    | ChangeRequestParameterValue Int String
    | DeleteRequestParameter Int
    | AddRequestHeader
    | ChangeRequestHeaderAttribute String Int String
    | DeleteRequestHeader Int
    | OnLocationChange Location
    | ShowRawResponse
    | ShowFormattedResponse
    | UpdateRequestBody RequestBody
    | RemoveRequestBody
