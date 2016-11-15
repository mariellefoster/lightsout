import Html.App as App
import Html exposing (Html, div, text, node)
import Html.Attributes exposing (style, id, class, rel, href)
import Window
import Task
import Time
import Process
import Timer
import Board

main =
    App.program
        { init = init -----values----
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model =
    { board : Board.Model
    , windowSize : Window.Size
    , timer : Timer.Model
    , difficulty : Board.DifficultyLevel
    }

init : (Model, Cmd Msg)
init =
    let
        difficulty = 1
        size = { width = 800, height = 800 }
        (newBoard, boardCmd) = Board.init difficulty
        (timer, timerCmd) = Timer.init
        model =
            { board = newBoard
            , windowSize = size
            , timer = timer
            , difficulty = difficulty
            }
        cmds = Cmd.batch
                [ (Cmd.map UpdateBoard boardCmd)
                , (Cmd.map UpdateTimer timerCmd)
                , getWindowSize
                ]
    in
        (model, cmds)

getWindowSize : Cmd Msg
getWindowSize = Task.perform SizeUpdateFailure NewWindowSize Window.size

newLevelCmd : Board.DifficultyLevel -> Cmd Msg
newLevelCmd d =
    Process.sleep (1 * Time.second)
    |> Task.perform (\_ -> NoOp) (\_ -> NewLevel d)
-- Update

type Msg
    = UpdateBoard Board.Msg
    | UpdateTimer Timer.Msg
    | NewLevel Board.DifficultyLevel
    | NoOp
    | NewWindowSize Window.Size
    | SizeUpdateFailure String

update : Msg -> Model -> (Model, Cmd Msg)
update message ({board, difficulty} as model) =
    case message of
        NewWindowSize newWindowSize -> ({ model | windowSize = newWindowSize }, Cmd.none)
        SizeUpdateFailure _ -> (model, Cmd.none)
        UpdateBoard boardMessage ->
            let
                (boardModel, cmd) = Board.update boardMessage model.board
                cmd2 =
                if Board.isWon boardModel.board then
                    newLevelCmd (difficulty + 1)
                else
                    Cmd.none
            in
                ({model | board = boardModel}, Cmd.batch [ Cmd.map UpdateBoard cmd, cmd2 ])
        UpdateTimer message ->
            let
                (newTimer, cmd) = Timer.update message model.timer
            in
                ({model | timer = newTimer}, Cmd.map UpdateTimer cmd)
        NewLevel difficulty ->
            let
                (newBoard, boardCmd) = Board.init difficulty
                (timer, timerCmd) = Timer.init
            in
                ({model | board = newBoard, difficulty = difficulty, timer = timer}
                , Cmd.batch [ Cmd.map UpdateBoard boardCmd, Cmd.map UpdateTimer timerCmd ]
                )
        NoOp -> (model, Cmd.none)



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions {timer} =
    let timerSubscription =
        timer
            |> Timer.subscriptions
            |> Sub.map UpdateTimer
    in
        Sub.batch [Window.resizes NewWindowSize, timerSubscription]

-- VIEW

view : Model -> Html Msg
view ({board, windowSize, timer, difficulty} as model) =
    let
        infoDivHeight = 80
        minSize = (Basics.min windowSize.width windowSize.height) - infoDivHeight |> toFloat
        lightCellSize = minSize / 5

        boardView =
            board
                |> Board.view lightCellSize
                |> App.map UpdateBoard

        mainDivStyle = style [ ("width", (toString minSize) ++ "px") ]
        infoDivStyle = style [ ("height", (toString infoDivHeight) ++ "px") ]
        innerDiv content = div [ class "infosection" ] content
        timerView =
            timer
                |> Timer.view
                |> App.map UpdateTimer
        infoDiv = div [ id "info", infoDivStyle ]
                      [ innerDiv [ text ("Time: "), timerView ]
                      , innerDiv [ text ("Moves: " ++ (toString board.moves)) ]
                      , innerDiv [ text ("Difficulty level: " ++ (toString difficulty)) ]
                      ]
    in
        div [ id "main", mainDivStyle ]
            [ css "style.css"
            , infoDiv
            , div [ class "board" ] [ boardView ]
            ]

css : String -> Html a
css path =
  node "link" [ rel "stylesheet", href path ] []
