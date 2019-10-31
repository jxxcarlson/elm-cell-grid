module CellGrid exposing
    ( CellGrid(..), CellType(..)
    , empty, initialize, repeat, fromList
    , map, mapWithIndex, foldl, transform, classifyCell, cellAtMatrixIndex, setValue, matrixIndex, matrixIndices
    , adjacent, neighbors
    )

{-| The CellGrid package provides a type for representing
a rectangular grid of cells. CellGrids can be created,
transformed, and rendered as either SVG or HTML.


## Types

@docs CellGrid, CellType


## Constructing and rendering CellGrids

@docs empty, initialize, repeat, fromList


## Work with cells

@docs map, mapWithIndex, foldl, transform, classifyCell, cellAtMatrixIndex, setValue, matrixIndex, matrixIndices

@docs adjacent, neighbors

-}

import Array exposing (Array)


{-| A value of type `CellGrid a` is a rectangular array
of values of type a.
-}
type CellGrid a
    = CellGrid ( Int, Int ) (Array a)


{-| A cell can be in the interior of the grid,
on an edge, on in a cornder
-}
type CellType
    = Corner
    | Edge
    | Interior


{-| The empty cell grid. Useful in conjunction with `Maybe.withDefault`
-}
empty : CellGrid a
empty =
    CellGrid ( 0, 0 ) (Array.fromList [])


{-| Construct a Maybe CellGrid from a list of values of type `a`.
Here is a 2x2 cell grid of type `Float`:

    > cg = fromList 2 2 [1.0,2.0,3.0,4.0]
      Just (CellGrid (2,2) (Array.fromList [1,2,3,4]))
      : Maybe (CellGrid Float)

If the length of the list is incompatible with the
given number of rows and columns `Nothing` is returned.

    > fromList 2 2 [1.0,2.0,3.0,4.0, 5.0]
      Nothing : Maybe (CellGrid Float)

-}
fromList : Int -> Int -> List a -> Maybe (CellGrid a)
fromList nRows nColumns data =
    case List.length data == nRows * nColumns of
        True ->
            Just <| CellGrid ( nRows, nColumns ) (Array.fromList data)

        False ->
            Nothing


{-| Map a function over a CellGrid:

    > map (\x -> 2*x) cg
    CellGrid (2,2) (Array.fromList [2,4,6,8])

-}
map : (a -> b) -> CellGrid a -> CellGrid b
map f (CellGrid ( nRows, nCols ) cells) =
    CellGrid ( nRows, nCols ) (Array.map f cells)


{-| Fold a reducer (a -> b -> b) over a CellGrid a)
-}
foldl : (a -> b -> b) -> b -> CellGrid a -> b
foldl reducer initialValue (CellGrid ( _, _ ) cells) =
    Array.foldl reducer initialValue cells


{-| Transform a CellGrid a with a function ((Int, Int) -> a -> a).
Used when the transformed value depends on its matrixIndex as well
as its value
-}
mapWithIndex : (( Int, Int ) -> a -> b) -> CellGrid a -> CellGrid b
mapWithIndex cellTransformer (CellGrid ( nRows, nCols ) cells) =
    let
        indexedCellTransformer =
            \k a -> cellTransformer (matrixIndex ( nRows, nCols ) k) a
    in
    CellGrid ( nRows, nCols ) (Array.indexedMap indexedCellTransformer cells)


{-| Transform a CellGrid using a function

    (Int, Int) -> CellGrid a -> a)

-}
transform : (( Int, Int ) -> CellGrid a -> b) -> CellGrid a -> CellGrid b
transform newCellValue ((CellGrid ( w, h ) elements) as grid) =
    let
        folder element ( ( i, j ), acc ) =
            ( nextIndex ( w, h ) ( i, j ), Array.push (newCellValue ( i, j ) grid) acc )
    in
    Array.foldl folder ( ( 0, 0 ), Array.empty ) elements
        |> Tuple.second
        |> CellGrid ( w, h )


nextIndex ( w, h ) ( i, j ) =
    if j < w - 1 then
        ( i, j + 1 )

    else
        ( i + 1, 0 )


{-| Give list of the call values at the eight neighgoring cells
-}
neighbors : ( Int, Int ) -> CellGrid a -> List a
neighbors ( x, y ) grid =
    List.filterMap (\index_ -> cellAtMatrixIndex index_ grid)
        [ ( x, y + 1 )
        , ( x - 1, y + 1 )
        , ( x - 1, y )
        , ( x - 1, y - 1 )
        , ( x, y - 1 )
        , ( x + 1, y - 1 )
        , ( x + 1, y )
        , ( x + 1, y + 1 )
        ]


{-| Give list of the cellValues at the four adjacent
-}
adjacent : ( Int, Int ) -> CellGrid a -> List a
adjacent ( x, y ) grid =
    List.filterMap (\index_ -> cellAtMatrixIndex index_ grid)
        [ ( x, y + 1 )
        , ( x - 1, y )
        , ( x, y - 1 )
        , ( x + 1, y )
        ]


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
a 2D array with n rows. Then `index  n (i,j)`
is the index in the 1D array of the corresponding
element in the 2D array at location (i,j)\`
-}
index : Int -> ( Int, Int ) -> Int
index nCols ( row, col ) =
    nCols * row + col


{-| Conversely, `(nRows, nCols) k` is the
2D array index `(i,j)` of the element al index `k`
-}
matrixIndex : ( Int, Int ) -> Int -> ( Int, Int )
matrixIndex ( nRows, nCols ) n =
    ( n // nCols, modBy nCols n )


{-| Return the Maybe value of the cell at (i,j) from grid

    > cellAtMatrixIndex (1,1) cg
    Just 4 : Maybe Float

-}
cellAtMatrixIndex : ( Int, Int ) -> CellGrid a -> Maybe a
cellAtMatrixIndex ( i, j ) grid =
    let
        (CellGrid ( _, nCols ) array) =
            grid
    in
    Array.get (index nCols ( i, j )) array


{-| Set the value of the cell at location (i,j)
-}
setValue : CellGrid a -> ( Int, Int ) -> a -> CellGrid a
setValue (CellGrid ( nRows, nCols ) values) ( i, j ) value =
    let
        k =
            index nRows ( i, j )
    in
    CellGrid ( nRows, nCols ) (Array.set k value values)


{-| return the type of the cell at location (i,j). Thus
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


{-| Return a list of all matrix indices (i,j) of a grid. Useful for mapping.

    grid : CellGrid
    grid =
        CellGrid.fromList 2 3 (List.range 0 5)
            |> Maybe.withDefault CellGrid.empty

    CellGrid.matrixIndices grid
        --> [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]

-}
matrixIndices : CellGrid a -> List ( Int, Int )
matrixIndices (CellGrid ( nRows, nCols ) _) =
    let
        -- because `List` is used, we must move from the bottom right corner to the top-left
        -- so intuitively everything is reversed.
        maxRow =
            nRows - 1

        maxColumn =
            nCols - 1

        go row column accum =
            if row < 0 then
                -- row is outside of range: stop
                accum

            else if column == 0 then
                -- last column of the row: decrement row, reset column
                go (row - 1) maxColumn (( row, column ) :: accum)

            else
                -- somewhere in the interior: decrement column
                go row (column - 1) (( row, column ) :: accum)
    in
    go maxRow maxColumn []


{-| Initialize a cell grid. `initialize (row, column) f` creates a
cell grid of size `(row, column)` with the element at `(i, j)` set to the result of `f (i, j)`.

    CellGrid.initialize (2,2) Tuple.first
        --> CellGrid.fromList 2 2 [ 0, 0, 1, 1 ]

    CellGrid.initalize ( 2, 3 ) (\( i, j ) -> toFloat (i + j))
        --> CellGrid.fromList 2 3 [ 0, 1, 2, 1, 2, 3 ]

-}
initialize : ( Int, Int ) -> (( Int, Int ) -> a) -> CellGrid a
initialize ( nRows, nCols ) temperatureMap =
    let
        maxRow =
            nRows - 1

        maxColumn =
            nCols - 1

        go row column accum =
            if row > maxRow then
                -- row is outside of range: stop
                accum

            else if column == maxColumn then
                -- last column of the row: increment row, reset column
                go (row + 1) 0 (Array.push (temperatureMap ( row, column )) accum)

            else
                -- somewhere in the interior: increment column
                go row (column + 1) (Array.push (temperatureMap ( row, column )) accum)
    in
    CellGrid ( nRows, nCols ) (go 0 0 Array.empty)


{-| Fill a cell grid with a constant value

    CellGrid.repeat (2,2) 42
        --> CellGrid.fromList 2 2 [ 42, 42, 42, 42 ]

-}
repeat : ( Int, Int ) -> a -> CellGrid a
repeat ( nRows, nCols ) value =
    let
        n =
            nRows * nCols
    in
    CellGrid ( nRows, nCols ) (Array.repeat n value)
