module Image2 exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import CellGrid exposing (CellGrid(..), Dimensions, Position, matrixIndex)
import CellGrid.RenderWebGL exposing (CellStyle, Vertex)
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Html.Lazy
import WebGL exposing (Mesh)


userScale =
    0.6


main : Program () Float Float
main =
    Browser.element
        { init = \_ -> ( 0, Cmd.none )
        , view = \model -> Html.Lazy.lazy2 view userScale model
        , subscriptions = \_ -> onAnimationFrameDelta Basics.identity
        , update = \elapsed currentTime -> ( elapsed + currentTime, Cmd.none )
        }


cellStyle : CellStyle Color
cellStyle =
    { toColor = identity
    , cellWidth = 0.1
    , cellHeight = 0.1
    }


view : Float -> Float -> Html msg
view scale t =
    let
        w =
            round (700 * scale)

        g =
            round (200 * scale)
    in
    CellGrid.RenderWebGL.asHtml { width = w, height = w } temperatureToColor (grid (Dimensions g g))


temperatureToColor : Float -> Color
temperatureToColor t =
    Color.rgb t 0 0


grid : Dimensions -> CellGrid Float
grid dimensions =
    let
        initializer i j =
            temperatureAtIndex dimensions (Position i j)
    in
    CellGrid.initialize dimensions initializer


temperatureAtIndex : Dimensions -> Position -> Float
temperatureAtIndex dimensions position =
    let
        iRatio =
            toFloat position.row / toFloat dimensions.rows

        jRatio =
            toFloat position.column / toFloat dimensions.columns

        pi =
            3.1416

        s1 =
            sin (8.7 * pi * iRatio)

        s2 =
            sin (4.1 * pi * jRatio)
    in
    0.5 + 0.5 * s1 * s2
