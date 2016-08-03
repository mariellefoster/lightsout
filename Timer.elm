module Timer exposing (..)

import Time exposing (Time, second)
import Html exposing (Html, text)
import Html.App as App
import Task
import Debug
import String

main =
    App.program
        { init = init -----values----
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model = 
    { startTime : Time
    , currentTime : Time 
    }

elapsedTime : Model -> Time
elapsedTime {startTime, currentTime} = currentTime - startTime

init : (Model, Cmd Msg)
init = ({startTime = 0, currentTime = 0}, getStartTime)

getStartTime : Cmd Msg
getStartTime = Task.perform TimeUpdateFailure StartTime Time.now





-- UPDATE

type Msg
    = Tick Time
    | TimeUpdateFailure String
    | StartTime Time

update : Msg -> Model -> (Model, Cmd Msg)
update message ({startTime, currentTime} as model) =
    let newModel =
        case message of
            Tick newTime ->
                (Debug.log ("got a tick" ++ (toString newTime))) {model | currentTime = newTime}
            StartTime startTime ->
                Debug.log "got a start" {model | startTime = startTime}
            _ -> Debug.log "got an error" model
    in
        (newModel, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick

-- VIEW

niceTimeDisplay : Time -> String
niceTimeDisplay time = 
    if time < 0 then 
        " "
    else
        let seconds = time
                    |> Time.inSeconds 
                    |> round
                    |> (\ x -> x % 60)
                    |> toString
                    |> String.padLeft 2 '0'
            minutes = time
                    |> Time.inMinutes
                    |> floor
                    |> toString
        in
        minutes ++ ":" ++ seconds

view : Model -> Html Msg
view model = model
        |> elapsedTime
        |> niceTimeDisplay
        |> text



