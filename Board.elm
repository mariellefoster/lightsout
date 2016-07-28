import Html exposing (Html, div)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes as SA
import Svg.Attributes exposing (..)
import Cell
import List
import SvgUtils
import Random
import Window
import Task
import Debug

--To Do--
    -- counts moves
    -- timer
    -- center of screen
    -- adjust to screen size
    -- css animations

    -- welcome page
    
    -- design levels
    
main =
    App.program
        { init = init -----values----
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model = 
    { board : Board
    , windowSize : Window.Size
    }

type alias Board = List (List Cell.Model)

init : (Model, Cmd Msg)
init = 
    let
        newBoard = Cell.init Cell.On
            |> List.repeat 5
            |> List.repeat 5
        size = { width = 800, height = 800 }
        model = { board = newBoard, windowSize = size }
        randomStartCmd = Random.generate NewBoard randomStart
        windowSizeCmd = getWindowSize
        cmds = Cmd.batch [randomStartCmd, windowSizeCmd]
    in
        (model, cmds)

getWindowSize : Cmd Msg
getWindowSize = Task.perform SizeUpdateFailure NewWindowSize Window.size

randomStart : Random.Generator Board
randomStart = Random.bool 
                |> Random.map (\b -> if b then Cell.On else Cell.Off) 
                |> Random.list 5
                |> Random.list 5


neighbors : Coords -> List Coords
neighbors (i, j) = [(i, j), (i-1, j), (i+1, j), (i, j-1), (i, j+1)]


isWon : Board -> Bool
isWon board = not (List.member Cell.On (List.concat board))

-- Update

type Msg
    = CellMessage Coords Cell.Msg
    | NewBoard Board
    | NewWindowSize Window.Size
    | SizeUpdateFailure String

update : Msg -> Model -> (Model, Cmd Msg)
update message model = 
    let
        newModel = 
            case message of
                NewBoard newBoard -> { model | board = newBoard }                
                CellMessage coords cellMsg ->
                    { model | board = 
                        (indexedMap (\ (i, j) cellModel -> 
                            if (List.member (i, j) (neighbors coords)) then 
                                (Cell.update cellMsg cellModel) 
                            else 
                                cellModel) model.board) 
                    }
                NewWindowSize newWindowSize -> { model | windowSize = newWindowSize }
                _ -> model
    in
        (newModel, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Window.resizes NewWindowSize


-- VIEW
type alias Coords = (Int, Int)

view : Model -> Html Msg
view ({board, windowSize} as model) =
    let
        baseSize = (Cell.size * 5)
        minSize = (Basics.min windowSize.width windowSize.height) |> toFloat
        scale = minSize / baseSize
                |> Debug.log "scale"
        size = (toString minSize)
        svgTree =
            svgView model
            |> SvgUtils.scale scale

        mainDivStyle =
            Html.Attributes.style 
            [ ("margin", "auto")
            , ("position", "relative")
            , ("width", size ++ "px")
            ]
    in
        if not (isWon board) then
            div [mainDivStyle]
            [ svg [viewBox ("0 0 " ++ size ++ " " ++ size)] [svgTree] ]
        else
            text "You won! <3"

svgView : Model -> Svg Msg
svgView model =
    let 
        nodes = indexedMap renderCell model.board
        flattenedNodes = List.concat nodes
    in
        g [] flattenedNodes
 
renderCell : Coords -> Cell.Model -> Svg Msg
renderCell (i,j) cellModel =
    cellModel 
        |> Cell.svgView 
        |> SvgUtils.translate (Cell.size*i) (Cell.size*j)
        |> App.map (CellMessage (i, j))
            


indexedMap : (Coords -> a -> b) -> List (List a) -> List (List b)
indexedMap f board =
    board |> List.indexedMap (\ i row -> row |> List.indexedMap (\ j cellModel -> f (i, j) cellModel))




