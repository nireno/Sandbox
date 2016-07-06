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
    { init = init getter
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

getter = 
    { url = "http://localhost:3000/project?select=id,name"
    , fields = ["id", "name"]
    , prettyFields = ["ID", "Name"]
    }

init getter = 
    (
        { things = []
        , newThingName = ""
        , selectedThingId = 0
        , prevSelectedThingId = 0
        , getter = getter
        }
    , cmdGetThings getter.fields getter.url
    )

type Msg
  = FetchSucceed (List Thing)
  | FetchFail Http.Error
  --| Change String
  --| PostSucceed Http.Response
  --| PostFail Http.RawError
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
    , getter: 
        { url: String
        , fields: List String
        , prettyFields : List String
        }
    }

type alias Thing = List String

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    FetchSucceed things -> ({ model | things = things}, Cmd.none)
    FetchFail _ -> (model, Cmd.none)
    --Change str -> ({ model | newThingName = str }, Cmd.none)
    --PostSucceed _ -> (model, cmdGetThings model.thingUrl)
    --PostFail _-> (model, Cmd.none)
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
                ( tr [] (List.map (\prettyField -> th [] [text prettyField]) model.getter.prettyFields) :: List.map
                    (\thing -> 
                        tr [] (List.map (\field -> td [] [text field]) thing) )
                    model.things
                )
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
        Task.perform FetchFail FetchSucceed (Http.get decoder url)

--cmdPostThing : String -> String -> Cmd Msg
--cmdPostThing url thingName =
--    Task.perform PostFail PostSucceed (postThing url thingName)



jsonifyThing name =
    Json.Encode.object [("name", Json.Encode.string name )]
        |> Json.Encode.encode 0
        |> Http.string

--postThing url thingName =
--    Http.send Http.defaultSettings
--        { verb = "POST"
--        , headers = [("Content-Type", "application/json")]
--        , url = url
--        , body = jsonifyThing thingName
--        }

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