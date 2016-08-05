module Board exposing (..)

import Html exposing (Html, div, table, tr, td, text)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Cell
import List
import Random
import Window
import Task
import Timer


-- MODEL

type alias Coords = (Int, Int)

type alias Board = List (List Cell.Model)

type alias Model = { board: Board, moves: Int }

type alias DifficultyLevel = Int

init : DifficultyLevel -> (Model, Cmd Msg)
init difficulty =
    let
        randomStartCmd = Random.generate NewBoard (randomStart difficulty)
        model = { board = emptyBoard, moves = 0 }
    in
        (model, randomStartCmd)

emptyBoard =
    Cell.init Cell.Off
        |> List.repeat 5
        |> List.repeat 5

randomCoords : Random.Generator Coords
randomCoords =
    let
       randomCoord = Random.int 0 4
    in
       Random.pair randomCoord randomCoord

randomStart : DifficultyLevel -> Random.Generator Board
randomStart difficulty =
    randomCoords
        |> Random.list difficulty
        |> Random.map (List.foldl toggleAt emptyBoard)


neighbors : Coords -> List Coords
neighbors (i, j) = [(i, j), (i-1, j), (i+1, j), (i, j-1), (i, j+1)]


isWon : Board -> Bool
isWon board = not (List.member Cell.On (List.concat board))

indexedMap : (Coords -> a -> b) -> List (List a) -> List (List b)
indexedMap f board =
    board
        |> List.indexedMap (\ i row -> row |> List.indexedMap (\ j cellModel -> f (i, j) cellModel))

toggleAt : Coords -> Board -> Board
toggleAt coords board =
    indexedMap (\ (i, j) cellModel ->
        if (List.member (i, j) (neighbors coords)) then
            (Cell.update Cell.Toggle cellModel)
        else
            cellModel
    ) board


-- Update

type Msg
    = ToggleAt Coords Cell.Msg
    | NewBoard Board

update : Msg -> Model -> (Model, Cmd Msg)
update message ({board, moves} as model) =
    let newModel =
        case message of
            NewBoard newBoard ->
                { model | board = newBoard }
            ToggleAt coords _ ->
                { model | board = toggleAt coords board , moves = moves + 1 }
    in
       (newModel, Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- VIEW

view : Float -> Model -> Html Msg
view cellSize model =
    let
        lightCellSize = toString cellSize
        cellStyle =
            style
                [ ("width", lightCellSize ++ "px")
                , ("height", lightCellSize ++ "px")
                ]
        rows = List.indexedMap
                (\i row -> tr [] (row |> List.indexedMap (\j cellModel -> td [ cellStyle ] [ (renderCell (i, j) cellModel) ])))
                model.board
        lightsTable = table [] rows

    in
       table [] rows

renderCell : Coords -> Cell.Model -> Html Msg
renderCell (i,j) cellModel =
    cellModel
        |> Cell.view
        |> App.map (ToggleAt (i, j))
