module CellGrid
    exposing
        ( CellGrid(..)
        , classifyCell
        , CellType(..)
        , CellRenderer
        , map
        , location
        , index
        , cellAtIndex
        , setValue
        , indices
        , renderAsHtml
        )

{-| This library is just a test. I repeat: a test!

## Main API

@docs CellGrid, CellType, renderAsHtml


## Work with cells

@docs map, classifyCell, cellAtIndex, setValue, location, index, indices

-}

import Array exposing (Array)
import Random
import List.Extra
import Svg exposing (Svg, svg, rect, g)
import Svg.Attributes as SA
import Html exposing (Html)


{-| A value of type `CellGrid a` is a rectangular array
of values of type a.

-}
type CellGrid a
    = CellGrid ( Int, Int ) (Array a)


{-| A cell can be in the interior of the grid,
on an edge, on in a cornder -}
type CellType
    = Corner
    | Edge
    | Interior

type alias ColorValue = String

type alias CellRenderer a = {
       cellSize : Float
     , cellColorizer : a -> ColorValue
     , defaultColor : ColorValue
  }

{-| Transform a CellGrid a with a cellTransformer (a -> a)
-}
map : ((Int,Int) -> a -> a) -> CellGrid a -> CellGrid a
map cellTransformer (CellGrid (nRows, nCols) cells) =
    let
        indexedCellTransformer = (\k a -> cellTransformer (index (nRows, nCols) k) a)
    in
   (CellGrid (nRows, nCols) (Array.indexedMap indexedCellTransformer cells))


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


{-| Return the Maybe value of the cell at (i,j) from grid

-}
cellAtIndex : ( Int, Int ) -> CellGrid a -> Maybe a
cellAtIndex ( i, j ) grid =
    let
        (CellGrid ( nRows, _ ) array) =
            grid
    in
        Array.get (location nRows ( i, j )) array


{-| Set the value of the cell at location (i,j)

-}
setValue : CellGrid a -> ( Int, Int ) -> a -> CellGrid a
setValue (CellGrid ( nRows, nCols ) values) ( i, j ) value =
    let
        k =
            location nRows ( i, j )
    in
        (CellGrid ( nRows, nCols ) (Array.set k value values))


{-| return the type of the cell at location (i,j).  Thus
classifyCell grid (0,0) = Corner
-}
classifyCell : CellGrid a -> ( Int, Int ) -> CellType
classifyCell cellGrid ( i, j ) =
    let
        ( nRows, nCols ) =
            dimensions cellGrid

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

{-| Return a list of all indices (i,j) of a grid.
Useful for mapping.

-}
indices : CellGrid a -> List ( Int, Int )
indices (CellGrid ( nRows, nCols ) _) =
    let
        n =
            nRows * nCols
    in
        List.map (index ( nRows, nCols )) (List.range 0 (n - 1))




--
-- RENDER GRID
--

{-| Render a cell grid as Html

-}
renderAsHtml : CellRenderer a -> CellGrid a -> Html msg
renderAsHtml cr cellGrid =
        svg
            [ SA.height <| String.fromFloat 400
            , SA.width <| String.fromFloat 400
            , SA.viewBox <| "0 0 400 400"
            ]
            [ renderAsSvg cr cellGrid ]



renderAsSvg : CellRenderer a -> CellGrid a -> Svg msg
renderAsSvg cr cellGrid =
    indices cellGrid
        |> List.map (renderCell cr cellGrid)
        |> g []


renderCell : CellRenderer a -> CellGrid a -> ( Int, Int ) -> Svg msg
renderCell cr cellGrid ( i, j ) =
    let
       color = Maybe.map cr.cellColorizer (cellAtIndex ( i, j ) cellGrid) |> Maybe.withDefault cr.defaultColor
    in
       gridRect cr.cellSize color ( i, j )



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
