import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Char
import String

rowDefs1 = 
    [ []
    , [6, 7]
    , [5,7,8]
    , [3,5,9]
    , [2]
    , [2,9,11]
    , [2,7]
    , [6,7,8,9]
    , [2,3,7,8,9]
    , [3,5,6,7,8,9,10]
    , [5,7,9]
    , [2,5,6,7]
    , [1,2,3,4,8]
    , [2,3,9]
    , [2,10]
    , [2,4,8,10]
    , [2,3,4,5,7,8,9,10]
    , [3,4,8,9]
    , [4,8,9]
    , [5,6,7,8,9,10]
    , [9]
    ]

rowDefs2 = 
    [ []
    , [2,3,4,6,7,8,9,10]
    , [2,4,5,6]
    , [2,5]
    , [2,10]
    , [2,9,10,11]
    , [2,7,10]
    , [2,3,6,7,8]
    , [3,4,7,9]
    , [2,3,6,8,9,10]
    , [2,3,4,8,9]
    , [6]
    , [1,3]
    , []
    , [2]
    , [2,4,8]
    , [2,3,4,5,7,8,9]
    , [2,4,8]
    , [2,9]
    , [2,3,4,5,6,7]
    , [9]
    ]

buildModel : List (List Int) -> List (Int, List (Int, Bool))
buildModel rowDefs = 
    let numRows = List.length rowDefs
        numCols = 11
        buildRow row offs currentCol = 
            if currentCol == numCols
                then row
                else buildRow (row ++ [(currentCol, if List.any (\idx -> idx == currentCol+1) offs then False else True)]) offs (currentCol+1)

        build model rowDefs currentRow = 
            case rowDefs of
                [] -> model
                first :: rest -> build (model ++ [(currentRow, buildRow [] first 0)]) rest (currentRow + 1)

    in build [] rowDefs 0

m1 = (buildModel rowDefs1, Cmd.none)
m2 = (buildModel rowDefs2, Cmd.none)

toggle model colIdx rowIdx = 
    List.map (\row -> 
        if rowIdx == fst row 
            then (rowIdx, List.map (\col ->
                if colIdx == fst col
                    then (colIdx, not (snd col))
                    else col) (snd row))
            else row ) model

toggle' model colIdx rowIdx =
    toggle (toggle (toggle (toggle (toggle model colIdx rowIdx) colIdx (rowIdx-1)) colIdx (rowIdx+1)) (colIdx-1) rowIdx) (colIdx+1) rowIdx

unicodeA = 97
letterToIdx letter = (Char.toCode letter) - unicodeA
idxToLetter idx = Char.fromCode (idx + unicodeA)

toggleSequence = [(8, 'q'), (4, 'q'), (2, 'm'), (9, 't'), (9, 'j'), (7, 'h'), (10, 'f'), (5, 'c'), (6, 'k'), (3, 'i')]

solve model sequence = 
    case sequence of 
        [] -> model
        first :: rest -> solve (toggle' model (fst first - 1) (letterToIdx (snd first))) rest


type alias Model = List( Int, List(Int, Bool) )
type Msg 
    = Toggle Int Int
    | Solve
    | Load Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Toggle colIdx rowIdx -> 
            let 
                newModel = toggle' model colIdx rowIdx
            in (newModel, Cmd.none)
        Solve ->
            (solve model toggleSequence, Cmd.none)
        Load i -> if i == 1 then m1 else m2

tdStyle isOn = 
    [ ("background-color", if isOn then "#B0BEC5" else "#4CAF50")
    , ("width", "16px")
    , ("height", "16px")
    , ("display", "inline-block")
    , ("border-radius", "10%")
    , ("margin", "2px")
    , ("font-size", "8px")
    ]

view : Model -> Html Msg
view model = 
    div [style [("background-color", "white"), ("text-align", "center")]] 
        [ button [onClick (Load 1)] [text "Page 1"]
        , button [onClick (Load 2)] [text "Page 2"]
        , table [] (List.map (\row -> tr [] (List.map (\col -> td [style (tdStyle (snd col)), onClick (Toggle  (fst col) (fst row) )] [text (toString (fst col + 1) ++ (String.cons (idxToLetter (fst row) ) "" ) ) ]) (snd row)) ) model)
        , button [onClick Solve] [text "solve"]
        , div [] [text (toString toggleSequence)]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main =
  Html.program
    { init = m1
    , view = view
    , update = update
    , subscriptions = subscriptions
    }