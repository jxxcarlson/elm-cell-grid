module CellGrid.RenderWebGL exposing
    ( asHtml
    , Vertex
    , CellStyle
    , meshFromCellGrid
    , meshToHtml
    , meshFromCellGridHelp
    )

{-| Render a `CellGrid` using WebGL.
See also the [examples](https://github.com/jxxcarlson/elm-cell-grid/tree/master/examples).

> **Note:** WebGL can only handle grids of about `100x100`.
> Drawing more cells exceeds the vertex limit, resulting in a partially blank image. `CellGrid.Image` can handle larger grids with ease.


## Rendering functions

@docs asHtml


## LowLevel

@docs Vertex
@docs CellStyle
@docs meshFromCellGrid
@docs meshToHtml


## Internals

Internal functions exposed for testing.

@docs meshFromCellGridHelp

-}

import Array
import CellGrid exposing (CellGrid(..), Position, matrixIndex)
import Color exposing (Color)
import Html
import Html.Attributes
import WebGL exposing (Mesh, Shader)


{-| Render a cell grid into an html element of the given width and height.

The cells are stretched to use up all available space. For customized cell sizes, see `meshToHtml`.

-}
asHtml : { width : Int, height : Int } -> (a -> Color) -> CellGrid a -> Html.Html msg
asHtml ({ width, height } as canvas) toColor ((CellGrid { rows, columns } _) as cellGrid) =
    let
        style : CellStyle a
        style =
            { toColor = toColor
            , cellWidth =
                0.1
                    / (toFloat width / toFloat columns)
            , cellHeight =
                0.1
                    / (toFloat height / toFloat rows)
            }

        mesh : Mesh Vertex
        mesh =
            meshFromCellGrid style cellGrid
    in
    meshToHtml canvas mesh



-- LOWLEVEL HTML


{-| Style an individual cell

    cellStyle : CellStyle Bool
    cellStyle =
        { toColor =
            \b ->
                if b then
                    Color.green

                else
                    Color.red
        , cellWidth = 10
        , cellHeight = 10
        }

-}
type alias CellStyle a =
    { toColor : a -> Color
    , cellWidth : Float
    , cellHeight : Float
    }


{-| Render a `Mesh Vertex` into an html element of the given width and height.
-}
meshToHtml : { width : Int, height : Int } -> WebGL.Mesh Vertex -> Html.Html msg
meshToHtml { width, height } mesh =
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



-- BUILD MESH


{-| An individual vertex.
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


{-| Create a mesh from a grid and a style for each cell.
-}
meshFromCellGrid : CellStyle a -> CellGrid a -> Mesh Vertex
meshFromCellGrid style cellGrid =
    meshFromCellGridHelp style cellGrid
        |> WebGL.triangles


{-| -}
meshFromCellGridHelp : CellStyle a -> CellGrid a -> List ( Vertex, Vertex, Vertex )
meshFromCellGridHelp style (CellGrid { rows, columns } array) =
    let
        folder : a -> ( Int, List ( Vertex, Vertex, Vertex ) ) -> ( Int, List ( Vertex, Vertex, Vertex ) )
        folder value ( index, accum ) =
            ( index - 1
            , addRectangleFromElement style ( matrixIndex { rows = rows, columns = columns } index, value ) accum
            )
    in
    Array.foldr folder ( Array.length array - 1, [] ) array
        |> Tuple.second


addRectangleFromElement :
    CellStyle a
    -> ( Position, a )
    -> List ( Vertex, Vertex, Vertex )
    -> List ( Vertex, Vertex, Vertex )
addRectangleFromElement style ( position, t ) accum =
    let
        x =
            -1.0 + toFloat position.column * style.cellWidth

        y =
            1.0 - toFloat position.row * style.cellHeight

        color =
            Color.toRgba (style.toColor t)

        v1 =
            ( Vertex x y 0 color.red color.green color.blue
            , Vertex (x + style.cellWidth) y 0 color.red color.green color.blue
            , Vertex x (y - style.cellHeight) 0 color.red color.green color.blue
            )

        v2 =
            ( Vertex (x + style.cellWidth) y 0 color.red color.green color.blue
            , Vertex (x + style.cellWidth) (y - style.cellHeight) 0 color.red color.green color.blue
            , Vertex x (y - style.cellHeight) 0 color.red color.green color.blue
            )
    in
    v1 :: v2 :: accum



-- SHADERS


vertexShader : Shader Vertex Uniforms { vr : Float, vg : Float, vb : Float }
vertexShader =
    [glsl|

        attribute float x;
        attribute float y;
        attribute float z;
        attribute float r;
        attribute float g;
        attribute float b;

        varying float vr;
        varying float vg;
        varying float vb;

        void main () {
            gl_Position = vec4(x, y, z, 1.0);
            vr = r;
            vg = g;
            vb = b;
        }

    |]


fragmentShader : Shader {} Uniforms { vr : Float, vg : Float, vb : Float }
fragmentShader =
    [glsl|

        precision mediump float;

        varying float vr;
        varying float vg;
        varying float vb;

        void main () {
            gl_FragColor = vec4(vr, vg, vb, 1.0);
        }

    |]
