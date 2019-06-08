module HeatMap exposing (main)

{-
   Rotating triangle, that is a "hello world" of the WebGL
-}

import Array
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


testMesh : Int -> Float -> Mesh Vertex
testMesh n ds =
    meshWithColorizer (colorAtMatrixIndex ( n, n )) ( n, n ) ( ds, ds )


testMesh2 : Int -> Float -> Mesh Vertex
testMesh2 n ds =
    testGrid ( n, n )
        |> meshFromCellGrid ( ds, ds ) redMap


meshWithColorizer : Colorizer -> ( Int, Int ) -> ( Float, Float ) -> Mesh Vertex
meshWithColorizer colorizer ( rows, cols ) ( dw, dh ) =
    let
        rect : ( Int, Int ) -> List ( Vertex, Vertex, Vertex )
        rect =
            rectangleAtIndex colorizer ( dw, dh )
    in
    List.range 0 (gridSize * gridSize - 1)
        |> List.map (matrixIndex ( gridSize, gridSize ))
        |> List.map rect
        |> List.concat
        |> WebGL.triangles


temperatureArray : CellGrid Float -> List ( ( Int, Int ), Float )
temperatureArray (CellGrid ( rows, cols ) array) =
    let
        idx =
            matrixIndex ( rows, cols )
    in
    Array.indexedMap (\k v -> ( idx k, v )) array
        |> Array.toList


redMap : Float -> Vec3
redMap t =
    vec3 (1.0 * t) 0 0


makeCellGrid : ( Int, Int ) -> (( Int, Int ) -> Float) -> CellGrid Float
makeCellGrid ( nRows, nCols ) temperatureMap =
    let
        n =
            nRows * nCols
    in
    List.map (matrixIndex ( nRows, nCols )) (List.range 0 (n - 1))
        |> List.map (\( i, j ) -> temperatureMap ( i, j ))
        |> (\list -> CellGrid ( nRows, nCols ) (Array.fromList list))


testGrid : ( Int, Int ) -> CellGrid Float
testGrid ( nRows, nCols ) =
    makeCellGrid ( nRows, nCols ) (temperatureAtIndex ( nRows, nCols ))


meshFromCellGrid : ( Float, Float ) -> (Float -> Vec3) -> CellGrid Float -> Mesh Vertex
meshFromCellGrid ( dw, dh ) temperatureMap cellGrid =
    let
        rect =
            rectangleFromElement temperatureMap ( dw, dh )
    in
    temperatureArray cellGrid
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


rectangleFromElement :
    (Float -> Vec3)
    -> ( Float, Float )
    -> ( ( Int, Int ), Float )
    -> List ( Vertex, Vertex, Vertex )
rectangleFromElement temperatureMap ( dw, dh ) ( ( i_, j_ ), t ) =
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
            temperatureMap t
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
    -- 0.5 + (0.25 * s1) + (0.25 * s2)
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
