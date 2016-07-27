module Cell exposing (..)

import Html exposing (Html)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes as SA
import Svg.Attributes exposing (..)



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

--init : Model
--init = On

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
    svg [viewBox "0 0 100 100", width "100px"] [(svgView model)]

svgView : Model -> Svg Msg
svgView model =
    let
        color =
            case model of
                On -> "#FFFF99"
                Off -> "grey"
    in
        --
        rect [ x "0", y "0", rx "15", ry "15", width (toString size)
             , height (toString size)
             , SA.style ("fill: " ++ color ++ "; stroke: white; stroke-width: 10")
             , onClick Toggle
             ] []


