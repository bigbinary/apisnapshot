module Update exposing (update)

import Array
import Http
import HttpMethods exposing (HttpMethod, parse, toString)
import Json.Decode
import JsonViewer
import JSVal
import Models exposing (Model, PageState(..))
import Msgs exposing (Msg)
import RequestParameters exposing (..)
import Router exposing (parseLocation)
import Set


urlWithEncodedParameters : String -> RequestParameters -> String
urlWithEncodedParameters url requestParameters =
    requestParameters
        |> Array.filter (\parameter -> parameter.name /= "")
        |> Array.map (\parameter -> Http.encodeUri parameter.name ++ "=" ++ Http.encodeUri parameter.value)
        |> Array.toList
        |> String.join "&"
        |> (++) (url ++ "?")


requestCommand : Model -> Cmd Msg
requestCommand model =
    let
        encodedUrl =
            urlWithEncodedParameters model.url model.requestParameters

        request =
            buildRequest encodedUrl model.httpMethod
    in
        Http.send Msgs.ResponseAvailable request


buildRequest : String -> HttpMethod -> Http.Request (Http.Response String)
buildRequest url httpMethod =
    Http.request
        { method = HttpMethods.toString httpMethod
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse preserveFullResponse
        , timeout = Nothing
        , withCredentials = False
        }


preserveFullResponse : Http.Response String -> Result String (Http.Response String)
preserveFullResponse resp =
    Ok resp


parseResponseBodyToJSVal : Http.Response String -> JSVal.JSVal
parseResponseBodyToJSVal httpResponse =
    let
        result =
            Json.Decode.decodeString JSVal.decoder httpResponse.body
    in
        case result of
            Ok jsonValue ->
                jsonValue

            Err err ->
                JSVal.JSString ("Error parsing the body. " ++ err)


updateModelWithResponse : Model -> Http.Response String -> Model
updateModelWithResponse model httpResponse =
    { model
        | pageState =
            Loaded
                { raw = httpResponse
                , collapsedNodePaths = Set.empty
                , json =
                    JsonViewer.fromJSVal (parseResponseBodyToJSVal httpResponse)
                }
    }


updateErrorResponse : Model -> Http.Error -> Model
updateErrorResponse model error =
    { model | pageState = Error error }


changeUrl : Model -> String -> Model
changeUrl model newUrl =
    { model | url = newUrl }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.ChangeUrl newUrl ->
            ( changeUrl model newUrl
            , Cmd.none
            )

        Msgs.Submit ->
            ( { model | pageState = Loading }, requestCommand model )

        Msgs.ResponseAvailable (Ok value) ->
            ( updateModelWithResponse model value, Cmd.none )

        Msgs.ResponseAvailable (Err error) ->
            ( updateErrorResponse model error, Cmd.none )

        Msgs.ToggleJsonCollectionView id ->
            ( case model.pageState of
                Loaded response ->
                    let
                        collapsedNodePaths =
                            response.collapsedNodePaths
                    in
                        if Set.member id collapsedNodePaths then
                            { model | pageState = Loaded { response | collapsedNodePaths = Set.remove id collapsedNodePaths } }
                        else
                            { model | pageState = Loaded { response | collapsedNodePaths = Set.insert id collapsedNodePaths } }

                _ ->
                    model
            , Cmd.none
            )

        Msgs.MoreActionsDropdownChange selectedOption ->
            case selectedOption of
                "Add Parameter" ->
                    update Msgs.AddRequestParameter model

                _ ->
                    ( model, Cmd.none )

        Msgs.AddRequestParameter ->
            ( { model | requestParameters = pushBlank model.requestParameters }
            , Cmd.none
            )

        Msgs.ChangeRequestParameterName index newName ->
            ( { model | requestParameters = updateName index newName model.requestParameters }
            , Cmd.none
            )

        Msgs.ChangeRequestParameterValue index newValue ->
            ( { model | requestParameters = updateValue index newValue model.requestParameters }
            , Cmd.none
            )

        Msgs.DeleteRequestParameter index ->
            ( { model | requestParameters = remove index model.requestParameters }, Cmd.none )

        Msgs.HttpMethodsDropdownChange selectedHttpMethodString ->
            ( { model | httpMethod = parse selectedHttpMethodString }, Cmd.none )

        Msgs.OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )
