import Html exposing (Html)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes as SA
import Svg.Attributes exposing (..)
import Cell
import List
import SvgUtils

--To Do--
    -- introduce randomness to initialize board
    -- "winning" state
    -- welcome page
    -- css animations
    -- design levels
    -- counts moves
    -- timer
    -- center of screen
    -- adjust to screen size

main =
    App.beginnerProgram
        { model = init -----values----
        , update = update
        , view = view
        }

-- Model

type alias Model = List (List Cell.Model)


init : Model
init = Cell.init Cell.On
        |> List.repeat 5
        |> List.repeat 5


neighbors : Coords -> List Coords
neighbors (i, j) = [(i, j), (i-1, j), (i+1, j), (i, j-1), (i, j+1)]

-- Update
type Msg
    = CellMessage Coords Cell.Msg

update : Msg -> Model -> Model
update message model = 
    case message of
        CellMessage coords cellMsg ->
            indexedMap (\ (i, j) cellModel -> if (List.member (i, j) (neighbors coords)) then (Cell.update cellMsg cellModel) else cellModel) model



-- View
type alias Coords = (Int, Int)

view : Model -> Html Msg
view model =
    let
        size = toString (Cell.size * 5)
    in
        svg [viewBox ("0 0 " ++ size ++ " " ++ size), width "500px"] [(svgView model)]

svgView : Model -> Svg Msg
svgView model =
    let 
        nodes = indexedMap renderCell model
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




