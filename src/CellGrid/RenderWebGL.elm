module CellGrid.RenderWebGL exposing
    ( Vertex
    , asHtml
    , meshFromCellGrid
    , meshWithColorizer
    , meshToHtml
    , meshWithColorizerHelp, meshFromCellGridHelp
    )

{-| The CellGrid.RenderWebGL package provides functions for rendereing CellGrid to WebGL


## Types

@docs Vertex


## Rendering functions

@docs asHtml


## Work with cells

@docs meshFromCellGrid


### recreating `meshWithColorizer`

@docs meshWithColorizer

A previous version of this package has a function:

    meshWithColorizer :
        (( Int, Int ) -> Color)
        -> ( Int, Int )
        ->
            { width : Float
            , height : Float
            }
        -> Mesh Vertex

This can be recreated using `CellGrid.initialize` and `meshFromCellGrid`:

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
@docs meshWithColorizerHelp, meshFromCellGridHelp

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


{-| The type of a function which assigns a color to a cell, the
latter given by its row and column indices
-}
type alias Colorizer =
    ( Int, Int ) -> Color


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


{-| Create a rows x cols Vertex Mesh representing an array of rectanagles of
size (dw, dh). The vertex colors (uniform over a rectangular cell) are determined by
the colorizer functionion which has type (Int, Int) -> Vec3
-}
meshWithColorizer : (( Int, Int ) -> Color) -> ( Int, Int ) -> { width : Float, height : Float } -> Mesh Vertex
meshWithColorizer colorizer position size =
    meshWithColorizerHelp colorizer position size
        |> WebGL.triangles


{-| -}
meshWithColorizerHelp : Colorizer -> ( Int, Int ) -> { width : Float, height : Float } -> List ( Vertex, Vertex, Vertex )
meshWithColorizerHelp colorizer ( rows, cols ) rectangle =
    let
        go i accum =
            if i >= 0 then
                go (i - 1) (addRectangleAtIndex colorizer rectangle (matrixIndex ( rows, cols ) i) accum)

            else
                accum
    in
    go (rows * cols - 1) []


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


addRectangleAtIndex :
    Colorizer
    -> { width : Float, height : Float }
    -> ( Int, Int )
    -> List ( Vertex, Vertex, Vertex )
    -> List ( Vertex, Vertex, Vertex )
addRectangleAtIndex colorizer rectangle ( i_, j_ ) accum =
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
            Color.toRgba (colorizer ( i_, j_ ))

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
