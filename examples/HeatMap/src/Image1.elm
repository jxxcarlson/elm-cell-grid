module Image1 exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import CellGrid exposing (CellGrid(..), matrixIndex)
import CellGrid.RenderWebGL exposing (Colorizer, Vertex)
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
    CellGrid.RenderWebGL.meshToHtml 700 700 (testMesh 200 0.04)


testMesh : Int -> Float -> Mesh Vertex
testMesh n ds =
    CellGrid.RenderWebGL.meshWithColorizer (colorAtMatrixIndex ( n, n )) ( n, n ) ( ds, ds )


redMap : Float -> Vec3
redMap t =
    vec3 (1.0 * t) 0 0


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
