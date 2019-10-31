module CellGrid exposing
    ( CellGrid(..)
    , empty, initialize, repeat, fromList
    , map, indexedMap
    , foldl, foldr, transform
    , get, set, update
    , adjacent, neighbors
    , CellType(..), classifyCell
    , arrayIndex, matrixIndex, matrixIndices
    )

{-| The CellGrid package provides a type for representing
a rectangular grid of cells. CellGrids can be created,
transformed, and rendered as either SVG or HTML.


## Type

@docs CellGrid


## Constructing and rendering CellGrids

@docs empty, initialize, repeat, fromList


## Array-like functions

@docs map, indexedMap
@docs foldl, foldr, transform


## Individual cell manipulation

@docs get, set, update
@docs adjacent, neighbors


## Cell types

@docs CellType, classifyCell


## Index helpers

@docs arrayIndex, matrixIndex, matrixIndices

-}

import Array exposing (Array)


{-| A value of type `CellGrid a` is a rectangular array
of values of type a.
-}
type CellGrid a
    = CellGrid Dimensions (Array a)


type alias Dimensions =
    { rows : Int
    , columns : Int
    }


type alias Position =
    { row : Int
    , column : Int
    }


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
    CellGrid (Dimensions 0 0) (Array.fromList [])


{-| Construct cell grid from a list of values.
Grid construction fails when an amount of values incompatible with the dimensions is given.

    fromList (Dimensions 2 2 ) [ 1.0, 2.0, 3.0, 4.0 ]
        --> Just (CellGrid (2,2) (Array.fromList [1,2,3,4]))

    fromList (Dimensions 2 2 ) [ 1.0, 2.0, 3.0, 4.0, 5.0 ]
        --> Nothing

-}
fromList : Dimensions -> List a -> Maybe (CellGrid a)
fromList dimensions data =
    if List.length data == dimensions.rows * dimensions.columns then
        Just <| CellGrid dimensions (Array.fromList data)

    else
        Nothing


{-| Map a function over a cell grid.

    cg : CellGrid Int
    cg = CellGrid.initialize (2,2) (\i j -> i + j)

    CellGrid.map (\x -> 2*x) cg
        --> CellGrid (2,2) (Array.fromList [2,4,6,8])

-}
map : (a -> b) -> CellGrid a -> CellGrid b
map f (CellGrid dimensions cells) =
    CellGrid dimensions (Array.map f cells)


{-| Fold a reducer (a -> b -> b) over a `CellGrid a`, associating to the left.
-}
foldl : (a -> b -> b) -> b -> CellGrid a -> b
foldl reducer initialValue (CellGrid _ cells) =
    Array.foldl reducer initialValue cells


{-| Fold a reducer (a -> b -> b) over a `CellGrid a`, associating to the right.
-}
foldr : (a -> b -> b) -> b -> CellGrid a -> b
foldr reducer initialValue (CellGrid _ cells) =
    Array.foldr reducer initialValue cells


{-| Transform a cell grid while having access to a cell's position

    indexedMap (\i j value -> value + i * j) cellGrid

-}
indexedMap : (Int -> Int -> a -> b) -> CellGrid a -> CellGrid b
indexedMap f (CellGrid dimensions cells) =
    let
        indexedCellTransformer index value =
            let
                ( i, j ) =
                    matrixIndex { rows = dimensions.rows, columns = dimensions.columns } index
            in
            f i j value
    in
    CellGrid dimensions (Array.indexedMap indexedCellTransformer cells)


{-| Modify a cell while having access to the full cell grid.
The supplied cell grid is always the original input. Useful for simulations that need neigbor information, like [Conway's game of life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).
-}
transform : (Int -> Int -> CellGrid a -> b) -> CellGrid a -> CellGrid b
transform newCellValue ((CellGrid dimensions elements) as grid) =
    let
        folder _ ( i, j, acc ) =
            let
                ( nextR, nextC ) =
                    if j < dimensions.columns - 1 then
                        ( i, j + 1 )

                    else
                        ( i + 1, 0 )
            in
            ( nextR, nextC, Array.push (newCellValue i j grid) acc )
    in
    Array.foldl folder ( 0, 0, Array.empty ) elements
        |> (\( _, _, value ) -> value)
        |> CellGrid dimensions


{-| Give list of the call values at the eight neighgoring cells
-}
neighbors : ( Int, Int ) -> CellGrid a -> List a
neighbors ( x, y ) grid =
    List.filterMap (\index_ -> get index_ grid)
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
    List.filterMap (\index_ -> get index_ grid)
        [ ( x, y + 1 )
        , ( x - 1, y )
        , ( x, y - 1 )
        , ( x + 1, y )
        ]



{-

   rows : CellGrid a -> Int
   rows (CellGrid ( rows_, _ ) _) =
       rows_


   cols : CellGrid a -> Int
   cols (CellGrid ( r_, cols_ ) _) =
       cols_


      dimensions : CellGrid a -> ( Int, Int )
      dimensions (CellGrid idx _) =
          idx
-}


{-| Consider a 1D array of elements which represents
a 2D array with n rows. Then `index  n (i,j)`
is the index in the 1D array of the corresponding
element in the 2D array at location (i,j)\`
-}
arrayIndex : { a | columns : Int } -> ( Int, Int ) -> Int
arrayIndex { columns } ( row, col ) =
    columns * row + col


{-| Conversely, `(dimensions.rows, dimensions.columns) k` is the
2D array index `(i,j)` of the element al index `k`
-}
matrixIndex : { rows : Int, columns : Int } -> Int -> ( Int, Int )
matrixIndex dimensions n =
    ( n // dimensions.columns, modBy dimensions.columns n )


{-| Return the Maybe value of the cell at (i,j) from grid

    > get (1,1) cg
    Just 4 : Maybe Float

-}
get : ( Int, Int ) -> CellGrid a -> Maybe a
get ( i, j ) grid =
    let
        (CellGrid dimensions array) =
            grid
    in
    Array.get (arrayIndex dimensions ( i, j )) array


{-| Set the value of the cell at location (i,j)
-}
set : ( Int, Int ) -> a -> CellGrid a -> CellGrid a
set ( i, j ) value (CellGrid dimensions values) =
    let
        k =
            arrayIndex dimensions ( i, j )
    in
    CellGrid dimensions (Array.set k value values)


{-| Update the value at a location `(row, column)`.
-}
update : ( Int, Int ) -> (a -> a) -> CellGrid a -> CellGrid a
update location updater ((CellGrid dimensions array) as cellGrid) =
    let
        position =
            arrayIndex dimensions location
    in
    case Array.get position array of
        Nothing ->
            cellGrid

        Just value ->
            CellGrid dimensions (Array.set position (updater value) array)


{-| return the type of the cell at location (i,j). Thus
classifyCell grid (0,0) = Corner

    cg : CellGrid Int
    cg = CellGrid.initialize (3,3) (\i j -> i + j)

    CellGrid.classifyCell (0 0) cg
        --> Corner

    CellGrid.classifyCell (0,1) cg
        --> Edge

    CellGrid.classifyCell (1,1) cg
        --> Interior

-}
classifyCell : CellGrid a -> ( Int, Int ) -> CellType
classifyCell cellGrid ( i, j ) =
    let
        (CellGrid dimensions _) =
            cellGrid

        mri =
            dimensions.rows - 1

        mci =
            dimensions.columns - 1
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
matrixIndices (CellGrid dimensions _) =
    let
        -- because `List` is used, we must move from the bottom right corner to the top-left
        -- so intuitively everything is reversed.
        maxRow =
            dimensions.rows - 1

        maxColumn =
            dimensions.columns - 1

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

    CellGrid.initialize (2,2)  (\i j -> i)
        --> CellGrid.fromList 2 2 [ 0, 0, 1, 1 ]

    CellGrid.initalize ( 2, 3 ) (\i j -> toFloat (i + j))
        --> CellGrid.fromList 2 3 [ 0, 1, 2, 1, 2, 3 ]

-}
initialize : Dimensions -> (Int -> Int -> a) -> CellGrid a
initialize dimensions temperatureMap =
    let
        maxRow =
            dimensions.rows - 1

        maxColumn =
            dimensions.columns - 1

        go row column accum =
            if row > maxRow then
                -- row is outside of range: stop
                accum

            else if column == maxColumn then
                -- last column of the row: increment row, reset column
                go (row + 1) 0 (Array.push (temperatureMap row column) accum)

            else
                -- somewhere in the interior: increment column
                go row (column + 1) (Array.push (temperatureMap row column) accum)
    in
    CellGrid dimensions (go 0 0 Array.empty)


{-| Fill a cell grid with a constant value

    CellGrid.repeat (2,2) 42
        --> CellGrid.fromList 2 2 [ 42, 42, 42, 42 ]

-}
repeat : Dimensions -> a -> CellGrid a
repeat dimensions value =
    let
        n =
            dimensions.rows * dimensions.columns
    in
    CellGrid dimensions (Array.repeat n value)
