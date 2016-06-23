module ProjectTasks exposing (Msg, Model, init, update, view, subscriptions)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (RawError, Response)
import Json.Decode exposing (..)
import Json.Encode
import Task exposing (Task)
import HttpBuilder
import String

main =
  Html.program
    { init = init 35
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias ProjectTask =
  { name: String
  }

type alias Model =
    { projectId: Int
    , tasks: List ProjectTask
    , newTaskName : String
    }

init projectId =
  (
    { projectId = projectId
    , tasks = []
    , newTaskName = ""
    }
  , cmdGetTasks projectId
  )


type Msg
  = FetchSucceed (List ProjectTask)
  | FetchFail Http.Error
  --| SwitchProject Int (List ProjectTask)
  | InputChange String
  --| Post String
  | PostSucceed Http.Response
  | PostFail Http.RawError
  | KeyUp Int


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    FetchSucceed tasks -> ({ model | tasks = tasks }, Cmd.none)
    FetchFail _ -> (model, Cmd.none)
    --SwitchProject projectId -> (model, switchProject)
    InputChange str -> ({ model | newTaskName = str }, Cmd.none)
    --Post newProjectName -> (model, cmdPostProject newProjectName)
    PostSucceed _ -> ({ model | newTaskName = "" }, cmdGetTasks model.projectId)
    PostFail _-> (model, Cmd.none)
    KeyUp keyCode ->
        if keyCode == 13 && String.length model.newTaskName > 0
            then (model, cmdPostTask model.projectId model.newTaskName)
            else (model, Cmd.none)


view : Model -> Html Msg
view model =
  div []
    [ input (inputAttribs model) []
    , ul [] (List.map (\task -> li [] [text task.name]) model.tasks )
    ]


inputAttribs model =
    [ placeholder "New task name..."
    , onInput InputChange
    , onKeyUp (\keyCode -> KeyUp keyCode)
    , Html.Attributes.value (if model.newTaskName == "" then "" else model.newTaskName) --clear input on create
    , autofocus True
    ]

onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
  on "keyup" (Json.Decode.map tagger keyCode)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


cmdGetTasks : Int -> Cmd Msg
cmdGetTasks projectId =
  let
    url = "http://localhost:3000/project_task?select=name&project_id=eq." ++ (toString projectId)
  in
    Task.perform FetchFail FetchSucceed (Http.get tasksDecoder url)

cmdPostTask : Int -> String -> Cmd Msg
cmdPostTask projectId taskName =
  let
    url = "http://localhost:3000/project_task"
  in
    Task.perform PostFail PostSucceed (postTask projectId taskName)

tasksDecoder : Json.Decode.Decoder (List ProjectTask)
tasksDecoder =
    Json.Decode.list (Json.Decode.object1 ProjectTask ("name" := string))

jsonifyTask : Int -> String -> Http.Body
jsonifyTask projectId name =
    Json.Encode.object
      [("name", Json.Encode.string name )
      , ("project_id", Json.Encode.int projectId)
      ]
        |> Json.Encode.encode 0
        |> Http.string

postTask : Int -> String -> Task RawError Response
postTask projectId taskName =
    Http.send Http.defaultSettings
        { verb = "POST"
        , headers = [("Content-Type", "application/json")]
        , url = "http://localhost:3000/project_task"
        , body = jsonifyTask projectId taskName
        }
