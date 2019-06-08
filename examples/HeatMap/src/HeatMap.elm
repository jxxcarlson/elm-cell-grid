module HeatMap exposing (main)

{-
   Rotating triangle, that is a "hello world" of the WebGL
-}

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import CellGrid exposing (CellGrid(..), matrixIndex)
import CellGrid.WebGL exposing (Colorizer, Vertex)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode exposing (Value)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)


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
    CellGrid.WebGL.toHtml 700 700 (testMesh 200 0.04)


testMesh : Int -> Float -> Mesh Vertex
testMesh n ds =
    CellGrid.WebGL.meshWithColorizer (colorAtMatrixIndex ( n, n )) ( n, n ) ( ds, ds )


testMesh2 : Int -> Float -> Mesh Vertex
testMesh2 n ds =
    testGrid ( n, n )
        |> CellGrid.WebGL.meshFromCellGrid ( ds, ds ) redMap


redMap : Float -> Vec3
redMap t =
    vec3 (1.0 * t) 0 0


testGrid : ( Int, Int ) -> CellGrid Float
testGrid ( nRows, nCols ) =
    CellGrid.WebGL.makeCellGrid ( nRows, nCols ) (temperatureAtIndex ( nRows, nCols ))


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
