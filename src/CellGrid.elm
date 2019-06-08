module CellGrid exposing
    ( CellGrid(..), CellType(..)
    , fromList, empty
    , map, mapWithIndex, foldl, transform, classifyCell, cellAtMatrixIndex, setValue
    , matrixIndex, matrixIndices
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
map : (a -> a) -> CellGrid a -> CellGrid a
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
mapWithIndex : (( Int, Int ) -> a -> a) -> CellGrid a -> CellGrid a
mapWithIndex cellTransformer (CellGrid ( nRows, nCols ) cells) =
    let
        indexedCellTransformer =
            \k a -> cellTransformer (matrixIndex ( nRows, nCols ) k) a
    in
    CellGrid ( nRows, nCols ) (Array.indexedMap indexedCellTransformer cells)


{-| Transform a CellGrid using a function

    (Int, Int) -> CellGrid a -> a)

-}
transform : (( Int, Int ) -> CellGrid a -> a) -> CellGrid a -> CellGrid a
transform newCellValue grid =
    List.foldl (\( i, j ) acc -> setValue acc ( i, j ) (newCellValue ( i, j ) grid)) grid (matrixIndices grid)


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
index nRows ( row, col ) =
    nRows * row + col


{-| Conversely, `matrixIndex (nRows, nCols) k` is the
2D array index `(i,j)` of the element al index `k`
-}
matrixIndex : ( Int, Int ) -> Int -> ( Int, Int )
matrixIndex ( nRows, nCols ) n =
    ( n // nCols, modBy nRows n )


{-| Return the Maybe value of the cell at (i,j) from grid

    > cellAtMatrixIndex (1,1) cg
    Just 4 : Maybe Float

-}
cellAtMatrixIndex : ( Int, Int ) -> CellGrid a -> Maybe a
cellAtMatrixIndex ( i, j ) grid =
    let
        (CellGrid ( nRows, _ ) array) =
            grid
    in
    Array.get (index nRows ( i, j )) array


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


{-| Return a list of all matrix indices (i,j) of a grid.
Useful for mapping.
-}
matrixIndices : CellGrid a -> List ( Int, Int )
matrixIndices (CellGrid ( nRows, nCols ) _) =
    let
        n =
            nRows * nCols
    in
    List.map (matrixIndex ( nRows, nCols )) (List.range 0 (n - 1))
