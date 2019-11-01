module Image1 exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import CellGrid exposing (CellGrid(..), Dimensions, Position, arrayIndex)
import CellGrid.RenderWebGL exposing (CellStyle, Vertex, meshFromCellGrid)
import Html exposing (Html)
import Json.Decode exposing (Value)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)
import Color exposing (Color)

main : Program Value Float Float
main =
    Browser.element
        { init = \_ -> ( 0, Cmd.none )
        , view = view
        , subscriptions = \_ -> onAnimationFrameDelta Basics.identity
        , update = \elapsed currentTime -> ( elapsed + currentTime, Cmd.none )
        }


view : Float -> Html msg
view t =
    CellGrid.RenderWebGL.meshToHtml { width = 400, height = 400 } mesh

dimensions : Dimensions
dimensions = Dimensions 10 10

initializer : Int -> Int -> Int
initializer i j = arrayIndex dimensions (Position i j)


cg : CellGrid Int
cg = CellGrid.initialize dimensions  (\i j -> i)


cellStyle : CellStyle Int
cellStyle =
    { toColor =
        \b ->
            if modBy 2 b == 0 then
                Color.black

            else
                Color.red
    , cellWidth = 40
    , cellHeight = 40
    }

mesh = meshFromCellGrid cellStyle cg



colorAtMatrixIndex : ( Int, Int ) -> ( Int, Int ) -> Vec3
colorAtMatrixIndex ( rows, cols ) ( i, j ) =
    let
        iRatio =
            toFloat i / toFloat rows

        jRatio =
            toFloat j / toFloat cols

        pi =
            3.1416

        s1 =
            sin (2.7 * pi * iRatio)

        s2 =
            sin (4.1 * pi * jRatio)
    in
    vec3 (0.5 + 0.3 * s1) 0.0 (0.5 + 0.5 * s2)

