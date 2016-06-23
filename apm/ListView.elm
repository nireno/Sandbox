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

main =
  Html.program
    { init = init "http://localhost:3000/project"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init thingUrl = (
    { things = []
    , newThingName = ""
    , selectedThingId = 0
    , prevSelectedThingId = 0
    , thingUrl = thingUrl
    }
    , cmdGetThings thingUrl)


type Msg
  = FetchSucceed (List Thing)
  | FetchFail Http.Error
  | Change String
  | PostSucceed Http.Response
  | PostFail Http.RawError
  | KeyUp Int
  | SelectThing Int
  | DeleteSucceed Http.Response
  | DeleteKey
  | OtherKey

type alias Model =
    { things : List Thing
    , newThingName : String
    , selectedThingId : Int
    , prevSelectedThingId : Int
    , thingUrl: String
    }

type alias Thing =
    { id : Int
    , name : String
    }

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    FetchSucceed things -> ({ model | things = things }, Cmd.none)
    FetchFail _ -> (model, Cmd.none)
    Change str -> ({ model | newThingName = str }, Cmd.none)
    PostSucceed _ -> (model, cmdGetThings model.thingUrl)
    PostFail _-> (model, Cmd.none)
    DeleteSucceed _ -> ({ model | selectedThingId = nextThingId model.things model.selectedThingId}, cmdGetThings model.thingUrl)
    KeyUp keycode ->
            if keycode == enterKeycode && String.length model.newThingName > 0 then
                ({ model | newThingName = "" }, cmdPostThing model.thingUrl model.newThingName)
            else (model, Cmd.none)
    SelectThing thingId ->
        (
            { model | prevSelectedThingId = model.selectedThingId, selectedThingId = thingId
            }
            , Cmd.none
        )
    DeleteKey ->
        (model, cmdDeleteThing model.thingUrl model.selectedThingId)
    OtherKey ->
        (model, Cmd.none)


view : Model -> Html Msg
view model =
    let listItemAttribs thingId =
        [ onClick (SelectThing thingId)
        , style
            [ ("cursor", "pointer")
            , if thingId == model.selectedThingId then ("color", "red") else ("color", "black")
            ]
        ]
    in
        div []
            [ input (inputAttribs model) []
            , ul [] (List.map (\thing -> li (listItemAttribs thing.id) [text thing.name]) model.things )
            ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups (\ keycode -> if keycode == deleteKeycode then DeleteKey else OtherKey) 

inputAttribs model =
    [ placeholder "Create new item..."
    , Html.Attributes.value model.newThingName
    , onInput Change
    , onKeyUp (\keyCode -> KeyUp keyCode)
    , autofocus True
    ]

onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
  on "keyup" (Json.Decode.map tagger keyCode)


cmdGetThings : String -> Cmd Msg
cmdGetThings url =
    Task.perform FetchFail FetchSucceed (Http.get thingsDecoder (url ++ "?select=id,name"))

cmdPostThing : String -> String -> Cmd Msg
cmdPostThing url thingName =
    Task.perform PostFail PostSucceed (postThing url thingName)

thingsDecoder =
    Json.Decode.list
        ( Json.Decode.object2 Thing ("id" := Json.Decode.int) ("name" := Json.Decode.string))

jsonifyThing name =
    Json.Encode.object [("name", Json.Encode.string name )]
        |> Json.Encode.encode 0
        |> Http.string

postThing url thingName =
    Http.send Http.defaultSettings
        { verb = "POST"
        , headers = [("Content-Type", "application/json")]
        , url = url
        , body = jsonifyThing thingName
        }

deleteThing url thingId =
    Http.send Http.defaultSettings
        { verb = "DELETE"
        , headers = [("Content-Type", "application/json")]
        , url = url
        , body = Http.string ""
        }

cmdDeleteThing : String -> Int -> Cmd Msg
cmdDeleteThing url thingId =
    Task.perform PostFail DeleteSucceed (deleteThing url thingId)

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