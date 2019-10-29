module Image2 exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import CellGrid exposing (CellGrid(..), matrixIndex)
import CellGrid.RenderWebGL exposing (Colorizer, Vertex)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode exposing (Value)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)

userScale = 0.5

main : Program Value Float Float
main =
    Browser.element
        { init = \_ -> ( 0, Cmd.none )
        , view = view userScale
        , subscriptions = \_ -> onAnimationFrameDelta Basics.identity
        , update = \elapsed currentTime -> ( elapsed + currentTime, Cmd.none )
        }


view : Float -> Float -> Html msg
view scale t =
    let
        w = round(scale*700)
        g = round(200*scale)
    in
    CellGrid.RenderWebGL.asHtml w w (grid ( g, g )) colorMap

--
--mesh : Int -> Float -> Mesh Vertex
--mesh n ds =
--    grid ( n, n )
--        |> CellGrid.RenderWebGL.meshFromCellGrid ( ds, ds ) colorMap


colorMap : Float -> Vec3
colorMap t =
    vec3 t 0 0


grid : ( Int, Int ) -> CellGrid Float
grid ( nRows, nCols ) =
    CellGrid.makeCellGrid ( nRows, nCols ) (temperatureAtIndex ( nRows, nCols ))


temperatureAtIndex : ( Int, Int ) -> ( Int, Int ) -> Float
temperatureAtIndex ( rows, cols ) ( i, j ) =
    let
        iRatio =
            toFloat i / toFloat rows

        jRatio =
            toFloat j / toFloat cols

        pi =
            3.1416

        s1 =
            sin (8.7 * pi * iRatio)

        s2 =
            sin (4.1 * pi * jRatio)
    in
    0.5 + 0.5 * s1 * s2
