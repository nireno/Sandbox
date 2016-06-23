import Projects
import ProjectTasks
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
import ShortcutKeys exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
    { projects : Projects.Model
    , projectTasks : ProjectTasks.Model
    }

type Msg
    = ProjectsWidget Projects.Msg
    | ProjectTasksWidget ProjectTasks.Msg

init =
    let (projectsModel, projectsCmd) = Projects.init
        (projectTasksModel, projectTasksCmd) = ProjectTasks.init 0
    in
        ( Model projectsModel projectTasksModel
        , Cmd.batch
            [ Cmd.map ProjectsWidget projectsCmd
            , Cmd.map ProjectTasksWidget projectTasksCmd
            ]
        )

update message model =
    case message of
        ProjectsWidget msg ->
            let (newProjects, projectsCmd) = Projects.update msg model.projects
                (newProjectTasks, projectTasksCmd) = ProjectTasks.init newProjects.selectedProjectId
                (newModel, cmds) = if newProjects.selectedProjectId == newProjects.prevSelectedProjectId
                    then
                        ({model | projects = newProjects}, Cmd.map ProjectsWidget projectsCmd)
                    else
                        ({model | projects = newProjects, projectTasks = newProjectTasks}, Cmd.batch [Cmd.map ProjectsWidget projectsCmd, Cmd.map ProjectTasksWidget projectTasksCmd])
            in
                (newModel, cmds)
        ProjectTasksWidget msg ->
            let (newProjectTasks, projectTasksCmd) = ProjectTasks.update msg model.projectTasks
            in ({model | projectTasks = newProjectTasks}, Cmd.map ProjectTasksWidget projectTasksCmd)

view : Model -> Html Msg
view model =
    div []
        [ Html.map ProjectsWidget (Projects.view model.projects)
        , Html.map ProjectTasksWidget (ProjectTasks.view model.projectTasks)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let 
        projectsSub = Projects.subscriptions model.projects
        projectTasksSub = ProjectTasks.subscriptions model.projectTasks
    in Sub.batch 
        [ Sub.map ProjectsWidget projectsSub
        , Sub.map ProjectTasksWidget projectTasksSub
        ]
