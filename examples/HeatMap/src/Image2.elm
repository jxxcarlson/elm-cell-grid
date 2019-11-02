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


main : Html msg
main =
    view


config = {dim = 100, size = 700}

cellStyle : CellStyle Color
cellStyle =
    { toColor = identity
    , cellWidth = 0.1
    , cellHeight = 0.1
    }


view : Html msg
view  =
     CellGrid.RenderWebGL.asHtml { width = config.size, height = config.size } temperatureToColor (grid (Dimensions config.dim config.dim))


temperatureToColor : Float -> Color
temperatureToColor t =
    Color.rgb t 0 (1 - 0.1 * t)


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
