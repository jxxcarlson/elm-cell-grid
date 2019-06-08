module HeatMap exposing (main)

{-
   Rotating triangle, that is a "hello world" of the WebGL
-}

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import CellGrid exposing (CellGrid(..), matrixIndex)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)


gridSize : Int
gridSize =
    100


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


type alias Uniforms =
    { perspective : Mat4 }


type alias Colorizer =
    ( Int, Int ) -> Vec3


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
    WebGL.toHtml
        [ width 400
        , height 400
        , style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            (testMesh 100 0.03)
            { perspective = Mat4.identity }
        ]



-- renderHeatMap : CellGrid Float -> Html msg
-- renderHeatMap ( CellGrid ( nRows, nCols ), array ) =
--     1
-- createGrid : (Int, Int) -> ((Int, Int) -> Vec3) -> Mesh Vertex
-- createGrid (nrows, ncols) colorizer =
--
--
-- mesh : Colorizer -> (Int, Int) -> (Float, Float) -> Mesh Vertex
-- mesh colorizer (rows, cols) (celWidth, cellHeight) =
-- makeMesh : (Int, Int)


testMesh : Int -> Float -> Mesh Vertex
testMesh n ds =
    mesh (colorAtMatrixIndex ( n, n )) ( n, n ) ( ds, ds )


mesh : Colorizer -> ( Int, Int ) -> ( Float, Float ) -> Mesh Vertex
mesh colorizer ( rows, cols ) ( dw, dh ) =
    let
        rect =
            rectangleAtIndex colorizer ( dw, dh )
    in
    List.range 0 (gridSize * gridSize - 1)
        |> List.map (matrixIndex ( gridSize, gridSize ))
        |> List.map rect
        |> List.concat
        |> WebGL.triangles


rectangleAtIndex : Colorizer -> ( Float, Float ) -> ( Int, Int ) -> List ( Vertex, Vertex, Vertex )
rectangleAtIndex colorizer ( dw, dh ) ( i_, j_ ) =
    let
        i =
            toFloat i_

        j =
            toFloat j_

        x =
            -1.0 + i * dw

        y =
            1.0 - j * dh

        color_ =
            colorizer ( i_, j_ )
    in
    [ ( Vertex (vec3 x y 0) color_
      , Vertex (vec3 (x + dw) y 0) color_
      , Vertex (vec3 x (y - dh) 0) color_
      )
    , ( Vertex (vec3 (x + dw) y 0) color_
      , Vertex (vec3 (x + dw) (y - dh) 0) color_
      , Vertex (vec3 x (y - dh) 0) color_
      )
    ]


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]



{- For testing vvvvvv -}


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
            sin (1.1 * pi * jRatio)
    in
    vec3 (0.5 + 0.3 * s1) 0.0 (0.5 + 0.5 * s2)
