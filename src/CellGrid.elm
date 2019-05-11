module CellGrid
    exposing
        ( CellGrid(..)
        , classifyCell
        , CellType(..)
        , location
        , index
        , cellAtIndex
        , setValue
        , indices
        , renderAsHtml
        )

{-| This library is just a test. I repeat: a test!

@docs location, index

-}

import Array exposing (Array)
import Random
import List.Extra
import Svg exposing (Svg, svg, rect, g)
import Svg.Attributes as SA
import Html exposing (Html)


type CellGrid a
    = CellGrid ( Int, Int ) (Array a)



type CellType
    = Corner
    | Edge
    | Interior

rows : CellGrid a -> Int
rows (CellGrid ( rows_, _ ) _) =
    rows_


cols : CellGrid a -> Int
cols (CellGrid ( r_, cols_ ) _) =
    cols_


dimensions : CellGrid a -> ( Int, Int )
dimensions (CellGrid idx _) =
    idx


{-| Consider a 1D array of elements which represents
a 2D array with n rows. Then `location  n (i,j)`
is the index in the 1D array of the correspondng
element in the 2D array at location (i,j)`
-}
location : Int -> ( Int, Int ) -> Int
location nRows ( row, col ) =
    nRows * row + col


{-| Conversely, `index (nRows, nCols) k` is the
2D array index `(i,j)` of the element al address `k`
-}
index : ( Int, Int ) -> Int -> ( Int, Int )
index ( nRows, nCols ) n =
    ( n // nCols, modBy nRows n )


cellAtIndex : ( Int, Int ) -> CellGrid a -> Maybe a
cellAtIndex ( i, j ) heatMap =
    let
        (CellGrid ( nRows, _ ) array) =
            heatMap
    in
        Array.get (location nRows ( i, j )) array


setValue : CellGrid a -> ( Int, Int ) -> a -> CellGrid a
setValue (CellGrid ( nRows, nCols ) values) ( i, j ) value =
    let
        k =
            location nRows ( i, j )
    in
        (CellGrid ( nRows, nCols ) (Array.set k value values))




classifyCell : CellGrid a -> ( Int, Int ) -> CellType
classifyCell heatMap ( i, j ) =
    let
        ( nRows, nCols ) =
            dimensions heatMap

        mri =
            nRows - 1

        mci =
            nCols - 1
    in
        case i == 0 || j == 0 || i == mri || j == mci of
            False ->
                Interior

            True ->
                if i == 0 && j == 0 then
                    Corner
                else if i == 0 && j == mci then
                    Corner
                else if i == mri && j == 0 then
                    Corner
                else if i == mri && j == mci then
                    Corner
                else
                    Edge



indices : CellGrid a -> List ( Int, Int )
indices (CellGrid ( nRows, nCols ) _) =
    let
        n =
            nRows * nCols
    in
        List.map (index ( nRows, nCols )) (List.range 0 (n - 1))




---
--- RNG
---
{-

   Example:

   > RNG.floatSequence 3 23 (0,1)
   [0.07049563320325747,0.8633668118636881,0.6762363032990798]

-}



--
-- RENDER GRID
--


renderAsHtml : CellGrid Float -> Html msg
renderAsHtml heatMap =
    let
        ( nr, nc ) =
            dimensions heatMap

        cellSize =
            400 / (toFloat nr)
    in
        svg
            [ SA.height <| String.fromFloat 400
            , SA.width <| String.fromFloat 400
            , SA.viewBox <| "0 0 400 400"
            ]
            [ renderAsSvg cellSize heatMap ]


renderAsSvg : Float -> CellGrid Float -> Svg msg
renderAsSvg cellSize heatMap =
    indices heatMap
        |> List.map (renderCell cellSize heatMap)
        |> g []


renderCell : Float -> CellGrid Float -> ( Int, Int ) -> Svg msg
renderCell cellSize heatMap ( i, j ) =
    let
        red =
            255.0 * (cellAtIndex ( i, j ) heatMap |> Maybe.withDefault 0)

        color =
            "rgb(" ++ String.fromFloat red ++ ", 0, 0)"
    in
        gridRect cellSize color ( i, j )


gridRect : Float -> String -> ( Int, Int ) -> Svg msg
gridRect size color ( row, col ) =
    rect
        [ SA.width <| String.fromFloat size
        , SA.height <| String.fromFloat size
        , SA.x <| String.fromFloat <| size * (toFloat col)
        , SA.y <| String.fromFloat <| size * (toFloat row)
        , SA.fill color

        --, SA.strokeWidth "1"
        -- , SA.stroke "rgb(25, 55, 125)"
        ]
        []
