module CellGrid.RenderWebGL exposing
    ( Colorizer, Vertex
    , asHtml, meshToHtml
    , meshFromCellGrid, meshWithColorizer
    , meshWithColorizerHelp, meshFromCellGridHelp
    )

{-| The CellGrid.RenderWebGL package provides functions for rendereing CellGrid to WebGL


## Types

@docs Colorizer, Vertex


## Rendering functions

@docs asHtml, meshToHtml


## Work with cells

@docs meshFromCellGrid, meshWithColorizer


## Lowlevel

@docs meshWithColorizerHelp, meshFromCellGridHelp

-}

import Array
import CellGrid exposing (CellGrid(..), matrixIndex)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)


{-| The type of vertices of triangles: define position and color.
-}
type alias Vertex =
    { x : Float
    , y : Float
    , z : Float
    , color : Vec3
    }


type alias Uniforms =
    { perspective : Mat4 }


{-| The type of a function which assigns a color to a cell, the
latter given by its row and column indices
-}
type alias Colorizer =
    ( Int, Int ) -> Vec3


{-| Render a WebGL "drawing" to a given rectangle on the screen.
-}
meshToHtml : Int -> Int -> WebGL.Mesh Vertex -> Html.Html msg
meshToHtml width_ height_ mesh =
    WebGL.toHtml
        [ width width_
        , height height_
        , style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            { perspective = Mat4.identity }
        ]


{-| Render a CellGrid to a width\_ x height\_ rectangle on the screen using
a function temperatureMap which transforms scalars to color vectors
-}
asHtml : Int -> Int -> CellGrid Float -> (Float -> Vec3) -> Html.Html msg
asHtml width_ height_ cellGrid temperatureMap =
    let
        (CellGrid ( nRows, nCols ) array) =
            cellGrid

        dw =
            toFloat width_
                / toFloat (250 * nRows)

        dh =
            toFloat height_
                / toFloat (250 * nCols)
    in
    WebGL.toHtml
        [ width width_
        , height height_
        , style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            (meshFromCellGrid ( dw, dh ) temperatureMap cellGrid)
            { perspective = Mat4.identity }
        ]



-- testMesh : Int -> Float -> Mesh Vertex
-- testMesh n ds =
--     testGrid ( n, n )
--         |> CellGrid.RenderWebGL.meshFromCellGrid ( ds, ds ) redMap


meshWithColorizer : Colorizer -> ( Int, Int ) -> ( Float, Float ) -> Mesh Vertex
meshWithColorizer colorizer position size =
    meshWithColorizerHelp colorizer position size
        |> WebGL.triangles


{-| Crreate a rows x cols Vertex Mesh representing an array of rectanagles of
size (dw, dh). The vertex colors (uniform over a rectangular cell) are determined by
the colorizer functionion which has type (Int, Int) -> Vec3
-}
meshWithColorizerHelp : Colorizer -> ( Int, Int ) -> ( Float, Float ) -> List ( Vertex, Vertex, Vertex )
meshWithColorizerHelp colorizer ( rows, cols ) ( dw, dh ) =
    let
        go i accum =
            if i >= 0 then
                go (i - 1) (addRectangleAtIndex colorizer ( dw, dh ) (matrixIndex ( rows, cols ) i) accum)

            else
                accum
    in
    go (rows * cols - 1) []


{-| Create a mesh from a cell grid using a temperatureMap. The latter
assigns to a temperature a triple of RGB values.
-}
meshFromCellGrid : ( Float, Float ) -> (Float -> Vec3) -> CellGrid Float -> Mesh Vertex
meshFromCellGrid size temperatureMap cellGrid =
    meshFromCellGridHelp size temperatureMap cellGrid
        |> WebGL.triangles


{-| Create a mesh from a cell grid using a temperatureMap. The latter
assigns to a temperature a triple of RGB values.
-}
meshFromCellGridHelp : ( Float, Float ) -> (Float -> Vec3) -> CellGrid Float -> List ( Vertex, Vertex, Vertex )
meshFromCellGridHelp ( dw, dh ) temperatureMap (CellGrid ( rows, cols ) array) =
    let
        folder : Float -> ( Int, List ( Vertex, Vertex, Vertex ) ) -> ( Int, List ( Vertex, Vertex, Vertex ) )
        folder value ( index, accum ) =
            ( index - 1
            , addRectangleFromElement temperatureMap ( dw, dh ) ( matrixIndex ( rows, cols ) index, value ) accum
            )
    in
    Array.foldr folder ( Array.length array - 1, [] ) array
        |> Tuple.second


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
    [ ( Vertex x y 0 color_
      , Vertex (x + dw) y 0 color_
      , Vertex x (y - dh) 0 color_
      )
    , ( Vertex (x + dw) y 0 color_
      , Vertex (x + dw) (y - dh) 0 color_
      , Vertex x (y - dh) 0 color_
      )
    ]


addRectangleAtIndex : Colorizer -> ( Float, Float ) -> ( Int, Int ) -> List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
addRectangleAtIndex colorizer ( dw, dh ) ( i_, j_ ) accum =
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

        v1 =
            ( Vertex x y 0 color_
            , Vertex (x + dw) y 0 color_
            , Vertex x (y - dh) 0 color_
            )

        v2 =
            ( Vertex (x + dw) y 0 color_
            , Vertex (x + dw) (y - dh) 0 color_
            , Vertex x (y - dh) 0 color_
            )
    in
    v1 :: v2 :: accum


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
    [ ( Vertex x y 0 color_
      , Vertex (x + dw) y 0 color_
      , Vertex x (y - dh) 0 color_
      )
    , ( Vertex (x + dw) y 0 color_
      , Vertex (x + dw) (y - dh) 0 color_
      , Vertex x (y - dh) 0 color_
      )
    ]


addRectangleFromElement :
    (Float -> Vec3)
    -> ( Float, Float )
    -> ( ( Int, Int ), Float )
    -> List ( Vertex, Vertex, Vertex )
    -> List ( Vertex, Vertex, Vertex )
addRectangleFromElement temperatureMap ( dw, dh ) ( ( i_, j_ ), t ) accum =
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

        v1 =
            ( Vertex x y 0 color_
            , Vertex (x + dw) y 0 color_
            , Vertex x (y - dh) 0 color_
            )

        v2 =
            ( Vertex (x + dw) y 0 color_
            , Vertex (x + dw) (y - dh) 0 color_
            , Vertex x (y - dh) 0 color_
            )
    in
    v1 :: v2 :: accum


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute float x;
        attribute float y;
        attribute float z;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * vec4(x, y, z, 1.0);
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
