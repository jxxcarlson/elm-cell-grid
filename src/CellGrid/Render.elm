module CellGrid.Render exposing (Msg, asHtml, asSvg, CellStyle)

{-| The CellGrid package provides a type for representing
a rectangular grid of cells. CellGrids can be created,
transformed, and rendered as either SVG or HTML.

@docs Msg, asHtml, asSvg, CellStyle

-}

import CellGrid exposing (CellGrid(..), Position)
import Color exposing (Color)
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (fill, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..))


{-| CellStyle is a record that provides the information --
size and color --
that is needed to render a cell to SVG. `Color` is as
defined in the package `avh4/elm-color`, e.g. `Color.rgb 1 0 0`,
which is bright red.
-}
type alias CellStyle a =
    { cellWidth : Float
    , cellHeight : Float
    , toColor : a -> Color
    , gridLineWidth : Float
    , defaultColor : Color
    , gridLineColor : Color
    }


{-| The MouseClick message sends the matrix index (i,j)
of the cell on which the user has clicked as well
as the local (x,y) coordinates of the cell.
-}
type alias Msg =
    { cell : Position
    , coordinates :
        { x : Float
        , y : Float
        }
    }


{-| Render a cell grid as Html. The first two parameters
are the width and height of the rendered grid in pixels.
-}
asHtml : { width : Int, height : Int } -> CellStyle a -> CellGrid a -> Html Msg
asHtml { width, height } cr cellGrid =
    svg
        [ TypedSvg.Attributes.InPx.height (toFloat height)
        , TypedSvg.Attributes.InPx.width (toFloat width)
        , TypedSvg.Attributes.viewBox 0 0 (toFloat width) (toFloat height)
        ]
        [ asSvg cr cellGrid ]


{-| Render a cell grid as an svg `<g>` element, useful for integration with other svg.
-}
asSvg : CellStyle a -> CellGrid a -> Svg Msg
asSvg style cellGrid =
    let
        elements =
            CellGrid.indexedMap (\i j -> renderCell style (Position i j)) cellGrid
                |> CellGrid.foldr (::) []
    in
    g [] elements


renderCell : CellStyle a -> Position -> a -> Svg Msg
renderCell style position value =
    let
        color =
            style.toColor value
    in
    rect
        [ width style.cellWidth
        , height style.cellHeight
        , x <| style.cellWidth * toFloat position.row
        , y <| style.cellHeight * toFloat position.column
        , fill (Fill color)
        , Mouse.onDown
            (\r ->
                let
                    ( x, y ) =
                        r.clientPos
                in
                { cell = position, coordinates = { x = x, y = y } }
            )
        , strokeWidth style.gridLineWidth
        , stroke style.gridLineColor
        ]
        []
