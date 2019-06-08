module CellGrid.Render exposing
    ( CellRenderer, Msg(..)
    , renderAsHtml, renderAsSvg
    )

{-| The CellGrid package provides a type for representing
a rectangular grid of cells. CellGrids can be created,
transformed, and rendered as either SVG or HTML.


## Types

@docs CellGrid, CellType, CellRenderer, Msg


## Constructing and rendering CellGrids

@docs fromList, empty, renderAsHtml, renderAsSvg


## Work with cells

@docs map, mapWithIndex, foldl, transform, classifyCell, cellAtMatrixIndex, setValue

-}

import Array exposing (Array)
import CellGrid exposing (CellGrid(..), cellAtMatrixIndex, matrixIndices)
import Color exposing (Color)
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (fill, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..))


{-| CellRenderer is a record that provides the information --
size and color --
that is needed to render a cell to SVG. `Color` is as
defined in the package `avh4/elm-color`, e.g. `Color.rgb 1 0 0`,
which is bright red.
-}
type alias CellRenderer a =
    { cellSize : Float
    , gridLineWidth : Float
    , cellColorizer : a -> Color
    , defaultColor : Color
    , gridLineColor : Color
    }


{-| The MouseClick message sends the matrix index (i,j)
of the cell on which the user has clicked as well
as the local (x,y) coordinates of the cell.
-}
type Msg
    = MouseClick ( Int, Int ) ( Float, Float )


{-| Render a cell grid as Html. The first two parameters
are the width and height of the rendered grid in pixels.
-}
renderAsHtml : Float -> Float -> CellRenderer a -> CellGrid a -> Html Msg
renderAsHtml width_ height_ cr cellGrid =
    svg
        [ height height_
        , width width_
        , viewBox 0 0 width_ height_
        ]
        [ renderAsSvg cr cellGrid ]


{-| Render a cell grid as SVG
-}
renderAsSvg : CellRenderer a -> CellGrid a -> Svg Msg
renderAsSvg cr cellGrid =
    matrixIndices cellGrid
        |> List.map (renderCell cr cellGrid)
        |> g []


renderCell : CellRenderer a -> CellGrid a -> ( Int, Int ) -> Svg Msg
renderCell cr cellGrid ( i, j ) =
    let
        size =
            cr.cellSize

        color =
            Maybe.map cr.cellColorizer (cellAtMatrixIndex ( i, j ) cellGrid) |> Maybe.withDefault cr.defaultColor
    in
    rect
        [ width size
        , height size
        , x <| size * toFloat i
        , y <| size * toFloat j
        , fill (Fill color)
        , Mouse.onDown (.clientPos >> MouseClick ( i, j ))
        , strokeWidth cr.gridLineWidth
        , stroke cr.gridLineColor
        ]
        []
