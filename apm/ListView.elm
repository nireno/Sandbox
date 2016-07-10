module ListView exposing (Msg, Model, init, update, view, subscriptions)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
import Json.Encode
import Task
import HttpBuilder
import String
import ShortcutKeys exposing (..)
import Keyboard
import Dict
import Maybe exposing (withDefault)

main =
  Html.program
    { init = init rpc_project
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

rpc_project = 
    { url = {get = "http://localhost:3000/project?select=id,name", set = "http://localhost:3000/project"}
    , fields = ["id", "name"]
    , prettyFields = ["ID", "Name"]
    }

rpc_task = 
    { url = {get = "http://localhost:3000/v_project_task?select=id,project,task", set = ""}
    , fields = ["id", "project", "task"]
    , prettyFields = ["ID", "Project", "Task"]
    }

init rpc = 
    (
        { things = []
        , newThingName = ""
        , selectedThingId = 0
        , prevSelectedThingId = 0
        , rpc = rpc
        }
    , cmdGetThings rpc.fields rpc.url.get
    )

type Msg
  = FetchSucceed (List Thing)
  | XhrFail Http.Error
  --| Change String
  | PostSucceed Http.Response
  | PostFail Http.RawError
  | ButtonClick Thing
  --| KeyUp Int
  --| SelectThing Int
  --| DeleteSucceed Http.Response
  --| DeleteKey
  --| OtherKey

type alias Model =
    { things : List Thing
    , newThingName : String
    , selectedThingId : Int
    , prevSelectedThingId : Int
    , rpc: 
        { url: { get: String, set: String}
        , fields: List String
        , prettyFields : List String
        }
    }

type alias Field =
    { name: String
    , prettyName: String
    , encoder: String -> Json.Encode.Value
    , editable: Bool
    }

type alias Thing = List String

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    FetchSucceed things -> ({ model | things = things}, Cmd.none)
    XhrFail _ -> (model, Cmd.none)
    --Change str -> ({ model | newThingName = str }, Cmd.none)
    PostSucceed _ -> (model, Cmd.none)
    PostFail _-> (model, Cmd.none)
    ButtonClick inputs -> (model, cmdPostThing model.rpc.url.set model.rpc.fields inputs)
    --DeleteSucceed _ -> ({ model | selectedThingId = nextThingId model.things model.selectedThingId}, cmdGetThings model.thingUrl)
    --KeyUp keycode ->
    --        if keycode == enterKeycode && String.length model.newThingName > 0 then
    --            ({ model | newThingName = "" }, cmdPostThing model.thingUrl model.newThingName)
    --        else (model, Cmd.none)
    --SelectThing thingId ->
    --    (
    --        { model | prevSelectedThingId = model.selectedThingId, selectedThingId = thingId
    --        }
    --        , Cmd.none
    --    )
    --DeleteKey ->
    --    (model, cmdDeleteThing model.thingUrl model.selectedThingId)
    --OtherKey ->
    --    (model, Cmd.none)


view : Model -> Html Msg
view model =
    let listItemAttribs thingId =
        [ {-onClick (SelectThing thingId)
        , -}style
            [ ("cursor", "pointer")
            , if thingId == model.selectedThingId then ("color", "red") else ("color", "black")
            ]
        ]
    in
        div []
            [ table []
                ( List.append 
                    (tr [] (List.map (\prettyField -> th [] [text prettyField]) model.rpc.prettyFields) 
                :: List.map
                    (\thing -> 
                        tr [] (List.map (\field -> td [] [text field]) thing) )
                    model.things)
                    [tr [] (List.map (\_ -> td [] [input [] []]) model.rpc.fields)]
                )
            , button [onClick (ButtonClick ["", "hi"])] [text "new thing"]
            , p [] [ jsonifyThing ["name"] ["niren"] |> toString |> text ]
            --, input (inputAttribs model) []
            --, ul [] (List.map (\thing -> li (listItemAttribs thing.id) [text thing.name]) model.things )
            ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
    --Keyboard.ups (\ keycode -> if keycode == deleteKeycode then DeleteKey else OtherKey) 

inputAttribs model =
    [ placeholder "Create new item..."
    , Html.Attributes.value model.newThingName
    --, onInput Change
    --, onKeyUp (\keyCode -> KeyUp keyCode)
    , autofocus True
    ]

--onKeyUp : (Int -> msg) -> Attribute msg
--onKeyUp tagger =
--  on "keyup" (Json.Decode.map tagger keyCode)


--cmdGetThings : String -> Cmd Msg
--cmdGetThings =
--    Task.perform FetchFail FetchSucceed (Http.get thingsDecoder (url ++ "?select=id,name"))

cmdGetThings : List String -> String -> Cmd Msg
cmdGetThings fields url =
    let
        stringify a = toString a
        something = 
            Json.Decode.oneOf 
            [ Json.Decode.string
            , Json.Decode.map stringify Json.Decode.int
            ]

        dictToList = Json.Decode.map (\d -> List.map (\f -> withDefault "?" (Dict.get f d)) fields) (Json.Decode.dict something)
        decoder = Json.Decode.list (dictToList)
    in
        Task.perform XhrFail FetchSucceed (Http.get decoder url)

jsonifyThing: List String -> Thing -> Http.Body
jsonifyThing fields thing =
    let 
        encodedThing = List.map Json.Encode.string thing
    in
        Json.Encode.object (List.map2 (,) fields encodedThing)
            |> Json.Encode.encode 0
            |> Http.string

postThing url fields thing =
    Http.send Http.defaultSettings
        { verb = "POST"
        , headers = [("Content-Type", "application/json")]
        , url = url
        , body = jsonifyThing fields thing
        }

cmdPostThing : String -> List String -> Thing -> Cmd Msg
cmdPostThing url fields thing =
    Task.perform PostFail PostSucceed (postThing url fields thing)





--deleteThing url thingId =
--    Http.send Http.defaultSettings
--        { verb = "DELETE"
--        , headers = [("Content-Type", "application/json")]
--        , url = url
--        , body = Http.string ""
--        }

--cmdDeleteThing : String -> Int -> Cmd Msg
--cmdDeleteThing url thingId =
--    Task.perform PostFail DeleteSucceed (deleteThing url thingId)

-- return the thing in the list that follows the thing with the given thingId 
nextThingId things thingId = 
    let fun ps id = 
        case ps of
            [] -> []
            hd::tl -> if hd.id == id then tl else fun tl id

    in 
        case fun things thingId of
            [] -> 0
            hd::tl -> hd.id