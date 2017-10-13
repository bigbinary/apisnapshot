module Pages.Preferences exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode exposing (encode)
import Json.Decode as Decode
import LocalStorageData exposing (..)
import Models
    exposing
        ( FirebaseConfig
        , FirebaseSdkInitializationState
        , firebaseConfigLocalStorageKey
        )
import Msgs exposing (PreferencesMsg(..))
import Json.Decode.Pipeline as DecodePipeline
import Ports


stringifyFirebaseConfig : FirebaseConfig -> String
stringifyFirebaseConfig firebaseConfig =
    let
        attributes =
            [ ( "apiKey", Encode.string firebaseConfig.apiKey )
            , ( "authDomain", Encode.string firebaseConfig.authDomain )
            , ( "databaseURL", Encode.string firebaseConfig.databaseURL )
            , ( "projectId", Encode.string firebaseConfig.projectId )
            , ( "storageBucket", Encode.string firebaseConfig.storageBucket )
            , ( "messagingSenderId", Encode.string firebaseConfig.messagingSenderId )
            ]
    in
        Encode.object attributes |> encode 0


firebaseConfigDecoder : Decode.Decoder FirebaseConfig
firebaseConfigDecoder =
    DecodePipeline.decode
        FirebaseConfig
        |> DecodePipeline.required "apiKey" Decode.string
        |> DecodePipeline.required "authDomain" Decode.string
        |> DecodePipeline.required "databaseURL" Decode.string
        |> DecodePipeline.required "projectId" Decode.string
        |> DecodePipeline.required "storageBucket" Decode.string
        |> DecodePipeline.required "messagingSenderId" Decode.string


decodeFirebaseConfig : String -> LocalStorageData String FirebaseConfig
decodeFirebaseConfig rawString =
    let
        result =
            Decode.decodeString firebaseConfigDecoder rawString
    in
        case result of
            Ok value ->
                LocalStorageData.Success value

            Err error ->
                LocalStorageData.Failure (Debug.log "" error)


update : PreferencesMsg -> FirebaseConfig -> ( FirebaseConfig, Cmd PreferencesMsg )
update msg model =
    case msg of
        ChangeApiKey newApiKey ->
            ( { model | apiKey = newApiKey }, Cmd.none )

        ChangeAuthDomain newAuthDomain ->
            ( { model | authDomain = newAuthDomain }, Cmd.none )

        ChangeDatabaseUrl newDatabaseUrl ->
            ( { model | databaseURL = newDatabaseUrl }, Cmd.none )

        ChangeProjectId newProjectId ->
            ( { model | projectId = newProjectId }, Cmd.none )

        ChangeStorageBucket newStorageBucket ->
            ( { model | storageBucket = newStorageBucket }, Cmd.none )

        ChangeMessagingSenderId newMessagingSenderId ->
            ( { model | messagingSenderId = newMessagingSenderId }, Cmd.none )

        FirebaseConfigSubmit ->
            let
                stringifiedFirebaseConfig =
                    stringifyFirebaseConfig model

                cmdValue =
                    { key = firebaseConfigLocalStorageKey
                    , value = stringifiedFirebaseConfig
                    }
            in
                ( model, Ports.localStorageSet cmdValue )


view : FirebaseConfig -> FirebaseSdkInitializationState -> Html PreferencesMsg
view config firebaseSdkInitializationState =
    div [ class "col-md-12" ]
        [ h2 [] [ text "Firebase Config" ]
        , viewFirebaseSdkInitiationState firebaseSdkInitializationState
        , Html.form [ class "col-md-6 form-horizontal", onSubmit FirebaseConfigSubmit ]
            [ viewInput "apiKey" config.apiKey ChangeApiKey
            , viewInput "authDomain" config.authDomain ChangeAuthDomain
            , viewInput "databaseURL" config.databaseURL ChangeDatabaseUrl
            , viewInput "projectId" config.projectId ChangeProjectId
            , viewInput "storageBucket" config.storageBucket ChangeStorageBucket
            , viewInput "messagingSenderId" config.messagingSenderId ChangeMessagingSenderId
            , div [ class "form-group" ]
                [ div [ class "col-sm-offset-2 col-sm-8" ]
                    [ button [ type_ "submit", class "btn btn-primary" ] [ text "Save" ]
                    ]
                ]
            ]
        ]


viewFirebaseSdkInitiationState : FirebaseSdkInitializationState -> Html PreferencesMsg
viewFirebaseSdkInitiationState state =
    case state of
        Models.Initializing ->
            p [ class "text-info" ] [ text "Firebase SDK is initializing" ]

        Models.Intialized ->
            p [ class "text-success" ] [ text "Firebase SDK is initialized" ]

        Models.InitializationError error ->
            p [ class "text-danger" ] [ text error ]


viewInput : String -> String -> (String -> PreferencesMsg) -> Html PreferencesMsg
viewInput labelText inputValue msg =
    div [ class "form-group" ]
        [ label [ class "col-sm-4 control-label" ] [ text labelText ]
        , div [ class "col-sm-8" ]
            [ input
                [ type_ "text"
                , class "form-control"
                , value inputValue
                , onInput msg
                ]
                []
            ]
        ]
