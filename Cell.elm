module Cell exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.App as App
import Html.Events exposing (onClick)

main =
    App.beginnerProgram
        { model = init On
        , update = update
        , view = view
        }

-- Model
type Light = On | Off

type alias Model = Light

init : Light -> Model
init l = l

-- Update
type Msg
    = Toggle

update : Msg -> Model -> Model
update message model =
    case model of
        On -> Off
        Off -> On


-- View

size = 100

view : Model -> Html Msg
view model =
    let
        color =
            case model of
                On -> "#FFFF99"
                Off -> "grey"
        divStyle =
            style
                [ ("background-color", color)
                , ("height", "95%")
                , ("width", "95%")
                , ("border-radius", "15px")
                ]
    in
       div [ divStyle, onClick Toggle ] []


