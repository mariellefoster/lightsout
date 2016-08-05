import Html.App as App
import Html exposing (Html, div, text, node)
import Html.Attributes exposing (style, id, class, rel, href)
import Window
import Task
import Timer
import Board

--To Do--

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

-- Update

type Msg
    = UpdateBoard Board.Msg
    | UpdateTimer Timer.Msg
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
            in
                if Board.isWon boardModel.board then
                   let
                       difficultyAbove = difficulty + 1
                       (newBoard, boardCmd) = Board.init difficultyAbove
                       (timer, timerCmd) = Timer.init
                   in
                       ({model | board = newBoard, difficulty = difficultyAbove, timer = timer}
                       , Cmd.batch [ Cmd.map UpdateBoard boardCmd, Cmd.map UpdateTimer timerCmd ]
                       )
                else
                    ({model | board = boardModel}, Cmd.map UpdateBoard cmd)
        UpdateTimer message ->
            let
                (newTimer, cmd) = Timer.update message model.timer
            in
                ({model | timer = newTimer}, Cmd.map UpdateTimer cmd)


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
            , div [ style [ ("flex-grow", "100") ] ] [ boardView ]
            ]

css : String -> Html a
css path =
  node "link" [ rel "stylesheet", href path ] []
