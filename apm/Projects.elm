module Projects exposing (Msg, Model, init, update, view, subscriptions)

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

projectRootUrl = "http://localhost:3000/project"

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init = (
    { projects = []
    , newProjectName = ""
    , selectedProjectId = 0
    , prevSelectedProjectId = 0
    }
    , cmdGetProjects)


type Msg
  = FetchSucceed (List Project)
  | FetchFail Http.Error
  | Change String
  | PostSucceed Http.Response
  | PostFail Http.RawError
  | KeyUp Int
  | SelectProject Int
  | DeleteSucceed Http.Response
  | DeleteKey
  | OtherKey

type alias Model =
    { projects : List Project
    , newProjectName : String
    , selectedProjectId : Int
    , prevSelectedProjectId : Int
    }

type alias Project =
    { id : Int
    , name : String
    }

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    FetchSucceed projects -> ({ model | projects = projects }, Cmd.none)
    FetchFail _ -> (model, Cmd.none)
    Change str -> ({ model | newProjectName = str }, Cmd.none)
    PostSucceed _ -> (model, cmdGetProjects)
    PostFail _-> (model, Cmd.none)
    DeleteSucceed _ -> ({ model | selectedProjectId = nextProjectId model.projects model.selectedProjectId}, cmdGetProjects)
    KeyUp keycode ->
            if keycode == enterKeycode && String.length model.newProjectName > 0 then
                ({ model | newProjectName = "" }, cmdPostProject model.newProjectName)
            else (model, Cmd.none)
    SelectProject projectId ->
        (
            { model | prevSelectedProjectId = model.selectedProjectId, selectedProjectId = projectId
            }
            , Cmd.none
        )
    DeleteKey ->
        (model, cmdDeleteProject model.selectedProjectId)
    OtherKey ->
        (model, Cmd.none)


view : Model -> Html Msg
view model =
    let projectItemAttribs projectId =
        [ onClick (SelectProject projectId)
        , style
            [ ("cursor", "pointer")
            , if projectId == model.selectedProjectId then ("color", "red") else ("color", "black")
            ]
        ]
    in
        div []
            [ input (inputAttribs model) []
            , ul [] (List.map (\project -> li (projectItemAttribs project.id) [text project.name]) model.projects )
            ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups (\ keycode -> if keycode == deleteKeycode then DeleteKey else OtherKey) 

inputAttribs model =
    [ placeholder "New project name..."
    , Html.Attributes.value model.newProjectName
    , onInput Change
    , onKeyUp (\keyCode -> KeyUp keyCode)
    , autofocus True
    ]

onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
  on "keyup" (Json.Decode.map tagger keyCode)


cmdGetProjects : Cmd Msg
cmdGetProjects =
  let
    url = "http://localhost:3000/project?select=id, name"
  in
    Task.perform FetchFail FetchSucceed (Http.get projectsDecoder url)

cmdPostProject : String -> Cmd Msg
cmdPostProject projectName =
  let
    url = "http://localhost:3000/project"
  in
    Task.perform PostFail PostSucceed (postProject projectName)

projectsDecoder =
    Json.Decode.list
        ( Json.Decode.object2 Project ("id" := Json.Decode.int) ("name" := Json.Decode.string))

jsonifyProject name =
    Json.Encode.object [("name", Json.Encode.string name )]
        |> Json.Encode.encode 0
        |> Http.string

postProject projectName =
    Http.send Http.defaultSettings
        { verb = "POST"
        , headers = [("Content-Type", "application/json")]
        , url = "http://localhost:3000/project"
        , body = jsonifyProject projectName
        }

deleteProject projectId =
    Http.send Http.defaultSettings
        { verb = "DELETE"
        , headers = [("Content-Type", "application/json")]
        , url = projectRootUrl ++ "?id=eq." ++ (toString projectId)
        , body = Http.string ""
        }

cmdDeleteProject : Int -> Cmd Msg
cmdDeleteProject projectId =
    Task.perform PostFail DeleteSucceed (deleteProject projectId)

-- return the project following the project with the given projectId 
nextProjectId projects projectId = 
    let fun ps id = 
        case ps of
            [] -> []
            hd::tl -> if hd.id == id then tl else fun tl id

    in 
        case fun projects projectId of
            [] -> 0
            hd::tl -> hd.id