module Image3 exposing (main)

{-| Render a cellgrid as a bmp image and display it using HTML
-}

import CellGrid exposing (CellGrid(..), Dimensions, Position)
import CellGrid.Image
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes


size : Int
size =
    100


main : Html msg
main =
    Html.img
        [ Html.Attributes.src (CellGrid.Image.asBmpUri grid)
        , Html.Attributes.width 600
        , Html.Attributes.height 600
        , Html.Attributes.style "image-rendering" "pixelated"
        ]
        []


grid : CellGrid Color
grid =
    let
        dimensions : Dimensions
        dimensions =
            Dimensions size size

        initializer : Int -> Int -> Color
        initializer i j =
            colorAtMatrixIndex dimensions (Position i j)
    in
    CellGrid.initialize dimensions initializer


colorAtMatrixIndex : Dimensions -> Position -> Color
colorAtMatrixIndex dimensions position =
    let
        -- a number in [0,1]
        x =
            toFloat position.row / toFloat dimensions.rows

        -- another number in [0,1]
        y =
            toFloat position.column / toFloat dimensions.columns

        pi =
            3.1416

        s1 =
            sin (3.7 * pi * x)

        s2 =
            sin (3.7 * pi * y)
    in
    Color.rgb (0.2 + 0.2 * s1*s2) 0.0 (0.5 + 0.5 * s1*s2)

