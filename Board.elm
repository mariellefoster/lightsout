import Html exposing (Html)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes as SA
import Svg.Attributes exposing (..)
import Cell
import List
import SvgUtils
import Random

--To Do--
    -- introduce randomness to initialize board
    -- "winning" state
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

type alias Model = List (List Cell.Model)


init : (Model, Cmd Msg)
init = 
    let
        model = Cell.init Cell.On
            |> List.repeat 5
            |> List.repeat 5
    in
        (model, Random.generate NewBoard randomStart)

randomStart : Random.Generator Model
randomStart = Random.bool 
                |> Random.map (\b -> if b then Cell.On else Cell.Off) 
                |> Random.list 5
                |> Random.list 5


neighbors : Coords -> List Coords
neighbors (i, j) = [(i, j), (i-1, j), (i+1, j), (i, j-1), (i, j+1)]

-- Update

type Msg
    = CellMessage Coords Cell.Msg
    | NewBoard Model

update : Msg -> Model -> (Model, Cmd Msg)
update message model = 
    let
        model = 
            case message of
                CellMessage coords cellMsg ->
                    indexedMap (\ (i, j) cellModel -> if (List.member (i, j) (neighbors coords)) then (Cell.update cellMsg cellModel) else cellModel) model
                NewBoard newModel -> newModel
    in
        (model, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- VIEW
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




