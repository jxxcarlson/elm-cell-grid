module Image1 exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import CellGrid exposing (CellGrid(..), Dimensions, Position)
import CellGrid.RenderWebGL exposing (CellStyle, Vertex, meshFromCellGrid)
import Color exposing (Color)
import Html exposing (Html)
import WebGL exposing (Mesh)


main : Program () Float Float
main =
    Browser.element
        { init = \_ -> ( 0, Cmd.none )
        , view = view
        , subscriptions = \_ -> onAnimationFrameDelta Basics.identity
        , update = \elapsed currentTime -> ( elapsed + currentTime, Cmd.none )
        }


view : Float -> Html msg
view t =
    CellGrid.RenderWebGL.meshToHtml { width = 1200, height = 1200 } mesh


cg : CellGrid Color
cg =
    let
        dimensions : Dimensions
        dimensions =
            Dimensions 100 100

        initializer : Int -> Int -> Color
        initializer i j =
            colorAtMatrixIndex dimensions (Position i j)
    in
    CellGrid.initialize dimensions initializer


cellStyle : CellStyle Color
cellStyle =
    { toColor = identity
    , cellWidth = 0.01
    , cellHeight = 0.01
    }


mesh : Mesh Vertex
mesh =
    meshFromCellGrid cellStyle cg


colorAtMatrixIndex : Dimensions -> Position -> Color
colorAtMatrixIndex dimensions position =
    let
        iRatio =
            toFloat position.row / toFloat dimensions.rows

        jRatio =
            toFloat position.column / toFloat dimensions.columns

        pi =
            3.1416

        s1 =
            sin (2.7 * pi * iRatio)

        s2 =
            sin (4.1 * pi * jRatio)
    in
    Color.rgb (0.5 + 0.3 * s1) 0.0 (0.5 + 0.5 * s2)
