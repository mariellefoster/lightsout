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

init : (Model, Cmd Msg)
init =
    let
        board = Cell.init Cell.On
            |> List.repeat 5
            |> List.repeat 5
        randomStartCmd = Random.generate NewBoard randomStart
        model = { board = board, moves = 0 }
    in
        (model, randomStartCmd)

randomStart : Random.Generator Board
randomStart = 
    Random.bool
        |> Random.map (\b -> if b then Cell.On else Cell.Off)
        |> Random.list 5
        |> Random.list 5


neighbors : Coords -> List Coords
neighbors (i, j) = [(i, j), (i-1, j), (i+1, j), (i, j-1), (i, j+1)]


isWon : Board -> Bool
isWon board = not (List.member Cell.On (List.concat board))

indexedMap : (Coords -> a -> b) -> List (List a) -> List (List b)
indexedMap f board =
    board
        |> List.indexedMap (\ i row -> row |> List.indexedMap (\ j cellModel -> f (i, j) cellModel))

-- Update

type Msg
    = ToggleAt Coords Cell.Msg
    | NewBoard Board

update : Msg -> Model -> (Model, Cmd Msg)
update message ({board, moves} as model) =
    let newModel =
        case message of
            NewBoard newBoard -> { model | board = newBoard }
            ToggleAt coords cellMsg ->
                { model
                    | board = indexedMap (\ (i, j) cellModel ->
                        if (List.member (i, j) (neighbors coords)) then
                            (Cell.update cellMsg cellModel)
                        else
                            cellModel
                        ) board
                    , moves = moves + 1
                }
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
