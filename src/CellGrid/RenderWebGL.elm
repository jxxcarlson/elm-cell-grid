module CellGrid.RenderWebGL exposing
    ( Vertex
    , asHtml
    , meshFromCellGrid
    , meshToHtml
    , meshFromCellGridHelp
    )

{-| The CellGrid.RenderWebGL module provides functions for rendereing CellGrid to WebGL


## Types

@docs Vertex


## Rendering functions

@docs asHtml


## Work with cells

@docs meshFromCellGrid


### recreating `meshWithColorizer`

A previous version of this package had a `meshWithColorizer` function.
It can be recreated using `CellGrid.initialize` and `meshFromCellGrid`:

    meshWithColorizer :
        (( Int, Int ) -> Color)
        -> ( Int, Int )
        ->
            { width : Float
            , height : Float
            }
        -> Mesh Vertex
    meshWithColorizer toColor size rectangle =
        CellGrid.initialize size toColor
            |> CellGrid.RenderWebGL.meshFromCellGrid rectangle identity


## Lowlevel

Internal functions exposed for testing.

@docs meshToHtml
@docs meshFromCellGridHelp

-}

import Array
import CellGrid exposing (CellGrid(..), matrixIndex)
import Color exposing (Color)
import Html
import Html.Attributes
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Mesh, Shader)


{-| The type of vertices of triangles: define position and color.
-}
type alias Vertex =
    { x : Float
    , y : Float
    , z : Float
    , r : Float
    , g : Float
    , b : Float
    }


type alias Uniforms =
    {}


{-| Render a WebGL "drawing" to a given rectangle on the screen.
-}
meshToHtml : Int -> Int -> WebGL.Mesh Vertex -> Html.Html msg
meshToHtml width height mesh =
    WebGL.toHtml
        [ Html.Attributes.width width
        , Html.Attributes.height height
        , Html.Attributes.style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            {}
        ]


{-| Render a CellGrid to a `width x height` rectangle.
-}
asHtml : Int -> Int -> CellGrid a -> (a -> Color) -> Html.Html msg
asHtml width height cellGrid temperatureMap =
    let
        (CellGrid ( nRows, nCols ) _) =
            cellGrid

        dw =
            toFloat width
                / toFloat (250 * nRows)

        dh =
            toFloat height
                / toFloat (250 * nCols)
    in
    WebGL.toHtml
        [ Html.Attributes.width width
        , Html.Attributes.height height
        , Html.Attributes.style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            (meshFromCellGrid { width = dw, height = dh } temperatureMap cellGrid)
            {}
        ]


{-| Create a mesh from a cell grid using a temperatureMap. The latter
assigns to a temperature a triple of RGB values.
-}
meshFromCellGrid : { width : Float, height : Float } -> (a -> Color) -> CellGrid a -> Mesh Vertex
meshFromCellGrid rectangle temperatureMap cellGrid =
    meshFromCellGridHelp rectangle temperatureMap cellGrid
        |> WebGL.triangles


{-| -}
meshFromCellGridHelp : { width : Float, height : Float } -> (a -> Color) -> CellGrid a -> List ( Vertex, Vertex, Vertex )
meshFromCellGridHelp rectangle temperatureMap (CellGrid ( rows, cols ) array) =
    let
        folder : a -> ( Int, List ( Vertex, Vertex, Vertex ) ) -> ( Int, List ( Vertex, Vertex, Vertex ) )
        folder value ( index, accum ) =
            ( index - 1
            , addRectangleFromElement temperatureMap rectangle ( matrixIndex ( rows, cols ) index, value ) accum
            )
    in
    Array.foldr folder ( Array.length array - 1, [] ) array
        |> Tuple.second


addRectangleFromElement :
    (a -> Color)
    -> { width : Float, height : Float }
    -> ( ( Int, Int ), a )
    -> List ( Vertex, Vertex, Vertex )
    -> List ( Vertex, Vertex, Vertex )
addRectangleFromElement temperatureMap rectangle ( ( i_, j_ ), t ) accum =
    let
        i =
            toFloat i_

        j =
            toFloat j_

        x =
            -1.0 + i * rectangle.width

        y =
            1.0 - j * rectangle.height

        color =
            Color.toRgba (temperatureMap t)

        v1 =
            ( Vertex x y 0 color.red color.green color.blue
            , Vertex (x + rectangle.width) y 0 color.red color.green color.blue
            , Vertex x (y - rectangle.height) 0 color.red color.green color.blue
            )

        v2 =
            ( Vertex (x + rectangle.width) y 0 color.red color.green color.blue
            , Vertex (x + rectangle.width) (y - rectangle.height) 0 color.red color.green color.blue
            , Vertex x (y - rectangle.height) 0 color.red color.green color.blue
            )
    in
    v1 :: v2 :: accum


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute float x;
        attribute float y;
        attribute float z;
        attribute float r;
        attribute float g;
        attribute float b;
        varying vec3 vcolor;

        void main () {
            gl_Position = vec4(x, y, z, 1.0);
            vcolor = vec3(r,g,b);
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
