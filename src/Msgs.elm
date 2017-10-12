module Msgs exposing (..)

import Http
import Navigation exposing (Location)


type Msg
    = Submit
    | ChangeUrl String
    | ResponseAvailable (Result Http.Error (Http.Response String))
    | ToggleJsonCollectionView String
    | MoreActionsDropdownChange String
    | HttpMethodsDropdownChange String
    | AddRequestParameter
    | AddAssertion
    | ChangeRequestParameterName Int String
    | ChangeRequestParameterValue Int String
    | DeleteRequestParameter Int
    | ChangeAssertionName Int String
    | ChangeAssertionValue Int String
    | DeleteAssertion Int
    | OnLocationChange Location
    | OnLocalStorageSet String
    | OnLocalStorageGet String
    | PreferencesMsg PreferencesMsg


type PreferencesMsg
    = ChangeApiKey String
    | ChangeAuthDomain String
    | ChangeDatabaseUrl String
    | ChangeProjectId String
    | ChangeStorageBucket String
    | ChangeMessagingSenderId String
    | FirebaseConfigSubmit
