module CellGrid.Render exposing
    ( asHtml, asSvg
    , Msg, CellStyle
    )

{-| Render a cell grid as html using SVG

SVG is slower for large cell grids, but is more interactive. User clicks on cells in the grid can be captured and used for interaction.

@docs asHtml, asSvg
@docs Msg, CellStyle

-}

import CellGrid exposing (CellGrid(..), Position)
import Color exposing (Color)
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Lazy



{-
   import Svg exposing (g, rect, svg)
   import Svg.Attributes exposing (fill, stroke, viewBox)
   import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width, x, y)
   import TypedSvg.Core exposing (Svg)
   import TypedSvg.Types exposing (Fill(..))

-}


{-| Customize how a cell is rendered.
`Color` is as defined in the package `avh4/elm-color`, e.g. `Color.rgb 1 0 0` is bright red.

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
        , gridLineWidth = 1
        , gridLineColor = Color.black
        }

-}
type alias CellStyle a =
    { cellWidth : Float
    , cellHeight : Float
    , toColor : a -> Color
    , gridLineWidth : Float
    , gridLineColor : Color
    }


{-| Capture clicks on the rendered cell grid. Gives the position in the cell grid, and the local `(x, y)` coordinates of the cell
-}
type alias Msg =
    { cell : Position
    , coordinates :
        { x : Float
        , y : Float
        }
    }


{-| Render a cell grid into an html element of the given width and height.
-}
asHtml : { width : Int, height : Int } -> CellStyle a -> CellGrid a -> Html Msg
asHtml { width, height } cr cellGrid =
    Svg.svg
        [ Svg.Attributes.height (String.fromInt height)
        , Svg.Attributes.width (String.fromInt width)
        , Svg.Attributes.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
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
    Svg.g [] elements


renderCell : CellStyle a -> Position -> a -> Svg Msg
renderCell style position value =
    Svg.rect
        [ Svg.Attributes.width (String.fromFloat style.cellWidth)
        , Svg.Attributes.height (String.fromFloat style.cellHeight)
        , Svg.Attributes.x (String.fromFloat (style.cellWidth * toFloat position.column))
        , Svg.Attributes.y (String.fromFloat (style.cellHeight * toFloat position.row))
        , Svg.Attributes.strokeWidth (String.fromFloat style.gridLineWidth)
        , Svg.Attributes.fill (toCssString (style.toColor value))
        , Svg.Attributes.stroke (toCssString style.gridLineColor)
        , Mouse.onDown
            (\r ->
                let
                    ( x, y ) =
                        r.clientPos
                in
                { cell = position, coordinates = { x = x, y = y } }
            )
        ]
        []


{-| Use a faster toCssString

Using `++` instead of `String.concat` which avh4/color uses makes this much faster.

-}
toCssString : Color -> String
toCssString color =
    let
        rgba =
            Color.toRgba color

        r =
            rgba.red

        g =
            rgba.green

        b =
            rgba.blue

        a =
            rgba.alpha

        pct x =
            ((x * 10000) |> round |> toFloat) / 100

        roundTo x =
            ((x * 1000) |> round |> toFloat) / 1000
    in
    "rgba("
        ++ String.fromFloat (pct r)
        ++ "%,"
        ++ String.fromFloat (pct g)
        ++ "%,"
        ++ String.fromFloat (pct b)
        ++ "%,"
        ++ String.fromFloat (roundTo a)
        ++ ")"
