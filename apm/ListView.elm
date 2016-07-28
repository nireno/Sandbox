module ListView exposing (Msg, Model, init, update, view, subscriptions)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (RawError, Response)
import Json.Decode
import Json.Encode
import Task exposing (Task)
import HttpBuilder
import String
import ShortcutKeys exposing (..)
import Keyboard
import Dict
import Maybe exposing (withDefault)

main =
  Html.program
    { init = init rpc_task
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

rpc_project = 
    { url = {get = "http://localhost:3000/project?select=id,name", set = "http://localhost:3000/project"}
    , fields = 
        [ {name = "id", value = "", prettyName = "ID", encoder = \s -> String.toInt s |> Result.withDefault 0 |> Json.Encode.int, editable=False}
        , {name = "name", value = "", prettyName = "Name", encoder = Json.Encode.string, editable=True}
        ]
    }

rpc_task = 
    { url = {get = "http://localhost:3000/project_task?select=id,project_id,name,estimated_hours", set = "http://localhost:3000/project_task"}
    , fields = 
        [ {name = "id", value = "", prettyName = "ID", encoder = Json.Encode.string, editable=False, hasMaster=False}
        , {name="project_id", value="", prettyName="Project", encoder = \s -> String.toInt s |> Result.withDefault 0 |> Json.Encode.int, editable=True, hasMaster=True}
        , {name="name", value="", prettyName="Task", encoder = Json.Encode.string, editable=True, hasMaster=False}
        , {name="estimated_hours", value="", prettyName="Estimate", encoder = \s -> String.toInt s |> Result.withDefault 0 |> Json.Encode.int, editable=True, hasMaster=False}
        ]
    , masters = [ {field = "project_id", url = "http://localhost:3000/project?select=id,name", items = []} ]
    }

init rpc = 
        { records = []
        , newRecordName = ""
        , selectedRecordId = 0
        , prevSelectedRecordId = 0
        , rpc = rpc
        }
        ! (cmdGetRecords rpc.fields rpc.url.get :: List.map (\m -> cmdGetMaster m) rpc.masters)

type Msg
  = FetchSucceed (List Record)
  | XhrFail Http.Error
  | Change String String
  | PostSucceed Http.Response
  | PostFail Http.RawError
  | ButtonClick
  | ReadMasterSucceed String (List (Int, String))
  --| KeyUp Int
  | SelectRecord (Maybe Int)
  | DeleteSucceed Http.Response
  | DeleteKey
  | EnterKey
  | NoOp

type alias Model =
    { records : List Record
    , newRecordName : String
    , selectedRecordId : Int
    , prevSelectedRecordId : Int
    , rpc: 
        { url: { get: String, set: String}
        , fields: List Field
        , masters: List MasterField
        }
    }

type alias Field =
    { name: String
    , value: String
    , prettyName: String
    , encoder: String -> Json.Encode.Value
    , editable: Bool
    , hasMaster: Bool
    }

fieldTemplate = {name="", value="", prettyName="", encoder=Json.Encode.string, editable=True, hasMaster=False}

type alias MasterField = 
    { field: String
    , url: String
    , items: List (Int, String)
    }

masterFieldTemplate = {field = "", url = "", items = []}

type alias Record = List String

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        FetchSucceed records -> ({ model | records = records}, Cmd.none)
        XhrFail _ -> (model, Cmd.none)
        Change fieldName str -> 
            let 
                fields = List.map (\f -> if f.name == fieldName then {f | value = str} else f) model.rpc.fields
            in
                ({ model | rpc = {url = model.rpc.url, fields = fields, masters = model.rpc.masters} }, Cmd.none)
        PostSucceed _ -> (model, cmdGetRecords model.rpc.fields model.rpc.url.get )
        PostFail _-> (model, Cmd.none)
        ButtonClick -> (model, cmdPostRecord model.rpc.url.set model.rpc.fields)
        ReadMasterSucceed fieldname pairs -> 
            let 
                masters = List.map (\m -> {m | items = if m.field == fieldname then pairs else m.items}) model.rpc.masters
                rpc = model.rpc
                newRpc = {rpc | masters = masters}
            in 
                ({model | rpc = newRpc}, Cmd.none)
        --DeleteSucceed _ -> ({ model | selectedRecordId = nextRecordId model.records model.selectedRecordId}, cmdGetRecords model.recordUrl)
        DeleteSucceed _ -> { model | selectedRecordId = 0} ! [cmdGetRecords model.rpc.fields model.rpc.url.get]
        --KeyUp keycode ->
        --        if keycode == enterKeycode && String.length model.newRecordName > 0 then
        --            ({ model | newRecordName = "" }, cmdPostRecord model.recordUrl model.newRecordName)
        --        else (model, Cmd.none)
        SelectRecord recordId -> {model | selectedRecordId = Maybe.withDefault 0 recordId} ! [Cmd.none]
        --    (
        --        { model | prevSelectedRecordId = model.selectedRecordId, selectedRecordId = recordId
        --        }
        --        , Cmd.none
        --    )
        DeleteKey ->
            (model, cmdDeleteRecord model.rpc.url.set model.selectedRecordId)
        EnterKey -> 
            (model, cmdPostRecord model.rpc.url.set model.rpc.fields)
        NoOp ->
            (model, Cmd.none)


view : Model -> Html Msg
view model =
    let 
        listItemAttribs recordId =
            [ {-onClick (SelectRecord recordId)
            , -}style
                [ ("cursor", "pointer")
                , if recordId == model.selectedRecordId then ("color", "red") else ("color", "black")
                ]
            ]

        masters = model.rpc.masters
        fields = model.rpc.fields
        tableHeaders = List.map (\f -> f.prettyName) fields
        getMaster : String -> Maybe MasterField
        getMaster fieldName = List.filter (\m -> m.field == fieldName) masters |> List.head

        masterDropdown fieldName =
            let 
                maybeMaster = getMaster fieldName
                msgOptionChanged val = Change fieldName val

                options = 
                    case maybeMaster of
                        Nothing -> []
                        Just master -> 
                            List.map 
                                (\(id, name) -> option [value (toString id)] [text name]) 
                                master.items
            in 
                select [onChange msgOptionChanged] options

        overlayMasters: Record -> Record
        overlayMasters record = 
            let 
                getMasterVal : List (Int, String) -> Int -> String
                getMasterVal masterItems key =
                    List.filter (\(k, _) -> k == key) masterItems 
                        |> List.head 
                        |> Maybe.withDefault (key, toString key)
                        |> \(_, val) -> val



                fldnames = List.map (\f -> f.name) fields

                overlayMaster : (String, String) -> String
                overlayMaster (fieldname, key) = 
                    case String.toInt key of
                        Result.Err _ -> key
                        Result.Ok i ->
                            case getMaster fieldname of 
                                Nothing -> key
                                Just master ->
                                    getMasterVal master.items i


                fldMap = List.map2 (\name val -> (name, val)) fldnames record
            in
                List.map overlayMaster fldMap

        getRecordId : Record -> Maybe Int
        getRecordId record = List.head record |> Maybe.withDefault "" |> String.toInt |> Result.toMaybe

        tdStyle selected = if selected then style [("border", "1px solid black")] else style []
    in
        div []
            [ table []
                ( List.append 
                    ( tr [] (List.map (\header -> th [] [text header]) tableHeaders) 
                        :: List.map
                            (\record -> 
                                tr 
                                    [onClick (getRecordId record |> SelectRecord)] 
                                    ( List.map 
                                        (\str -> 
                                            td [tdStyle ((getRecordId record |> Maybe.withDefault 0) == model.selectedRecordId)] [text str]) 
                                        (overlayMasters record)
                                    ) 
                            )
                            model.records 
                    )
                    [ tr []
                        ( List.map 
                            ( \f -> td []
                                [ if f.hasMaster 
                                    then masterDropdown f.name 
                                    else input [onInput (Change f.name), disabled (not f.editable)] []
                                ]
                            ) 
                            model.rpc.fields
                        )
                    ]
                )
            , button [onClick ButtonClick] [text "new record"]
            , div [] (List.map (\m -> p [] [text (toString m.items)]) masters)
            , div [] [text ( toString model.records) ]
            , div [] [text (toString model.selectedRecordId)]
            --, p [] [ jsonifyRecord ["name"] ["niren"] |> toString |> text ]
            --, input (inputAttribs model) []
            --, ul [] (List.map (\record -> li (listItemAttribs record.id) [text record.name]) model.records )
            ]

subscriptions : Model -> Sub Msg
subscriptions model =
    let tagger keycode =
        if keycode == deleteKeycode then DeleteKey 
        else if keycode == enterKeycode then EnterKey
        else NoOp
    in 
    Keyboard.ups tagger

inputAttribs model =
    [ placeholder "Create new item..."
    , Html.Attributes.value model.newRecordName
    --, onInput Change
    --, onKeyUp (\keyCode -> KeyUp keyCode)
    , autofocus True
    ]

onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
  on "keyup" (Json.Decode.map tagger keyCode)


--cmdGetRecords : String -> Cmd Msg
--cmdGetRecords =
--    Task.perform FetchFail FetchSucceed (Http.get recordsDecoder (url ++ "?select=id,name"))

decodeToString = 
    let 
        stringify a = toString a
    in 
        Json.Decode.oneOf 
        [ Json.Decode.string
        , Json.Decode.map stringify Json.Decode.int
        , Json.Decode.null "null"
        ]

cmdGetRecords : List Field -> String -> Cmd Msg
cmdGetRecords fields url =
    let
        fieldNames = List.map (\f -> f.name) fields

        dictToList = Json.Decode.map (\d -> List.map (\f -> withDefault "?" (Dict.get f d)) fieldNames) (Json.Decode.dict decodeToString)
        decoder = Json.Decode.list (dictToList)
    in
        Task.perform XhrFail FetchSucceed (Http.get decoder url)

cmdGetMaster : MasterField -> Cmd Msg
cmdGetMaster master =
    let
        dictToPair = Json.Decode.map (\d -> (Dict.get "id" d |> withDefault "0" |> String.toInt |> Result.withDefault 0, Dict.get "name" d |> withDefault "<error>") ) (Json.Decode.dict decodeToString)
        decoder = Json.Decode.list (dictToPair)
    in
        Task.perform XhrFail (ReadMasterSucceed master.field) (Http.get decoder master.url)


jsonifyRecord: List Field -> Http.Body
jsonifyRecord fields =
    let 
        editables = List.filter (\f -> f.editable) fields 
        fieldNames = List.map (\f -> f.name) editables
        fieldValues = List.map (\f -> f.value) editables
        fieldEncoders = List.map (\f -> f.encoder) editables
        bodyList = List.map3 (\name str encoder -> (name, encoder str)) fieldNames fieldValues fieldEncoders
    in
        Json.Encode.object bodyList
            |> Json.Encode.encode 0
            |> Http.string

postRecord: String -> List Field -> Task.Task RawError Response
postRecord url fields =
    Http.send Http.defaultSettings
        { verb = "POST"
        , headers = [("Content-Type", "application/json")]
        , url = url
        , body = jsonifyRecord fields
        }

cmdPostRecord : String -> List Field -> Cmd Msg
cmdPostRecord url fields =
    Task.perform PostFail PostSucceed (postRecord url fields)

cmdDeleteRecord : String -> Int -> Cmd Msg
cmdDeleteRecord url recordId =
    let 
        deleteRecord url recordId =
            Http.send Http.defaultSettings
                { verb = "DELETE"
                , headers = [("Content-Type", "application/json")]
                , url = url ++ "?id=eq." ++ toString recordId
                , body = Http.string ""
                }
    in
        Task.perform PostFail DeleteSucceed (deleteRecord url recordId)

-- return the record in the list that follows the record with the given recordId 
nextRecordId records recordId = 
    let fun ps id = 
        case ps of
            [] -> []
            hd::tl -> if hd.id == id then tl else fun tl id

    in 
        case fun records recordId of
            [] -> 0
            hd::tl -> hd.id

onChange : (String -> msg) -> Attribute msg
onChange tagger =
  on "change" (Json.Decode.map tagger targetValue)