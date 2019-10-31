module CellGrid exposing
    ( CellGrid(..), Dimensions, Position
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
transformed, and rendered with either SVG or WebGL.


## Type

@docs CellGrid, Dimensions, Position


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


{-| The size of a cell grid
-}
type alias Dimensions =
    { rows : Int
    , columns : Int
    }


{-| A position in a cell grid
-}
type alias Position =
    { row : Int
    , column : Int
    }


{-| A cell can be in the interior of the grid,
on an edge, on in a corner.
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

    import Array

    fromList (Dimensions 2 2 ) [ 1.0, 2.0, 3.0, 4.0 ]
        --> Just (CellGrid (Dimensions 2 2) (Array.fromList [1,2,3,4]))

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

    import Array

    dimensions : Dimensions
    dimensions = Dimensions 2 2

    initializer : Int -> Int -> Int
    initializer i j = arrayIndex dimensions (Position i j)

    cg : CellGrid Int
    cg = CellGrid.initialize dimensions initializer

    CellGrid.map (\x -> 2*x) cg
        --> CellGrid dimensions (Array.fromList [0,2,4,6])

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
                position =
                    matrixIndex { rows = dimensions.rows, columns = dimensions.columns } index
            in
            f position.row position.column value
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


{-| Get the list of cell values of the eight neighboring cells.

    dimensions : Dimensions
    dimensions = Dimensions 3 3

    initializer : Int -> Int -> Int
    initializer i j = arrayIndex dimensions (Position i j)

    cg : CellGrid Int
    cg = CellGrid.initialize dimensions initializer

    CellGrid.neighbors ( Position 1 1 )  cg
        --> [5,2,1,0,3,6,7,8]

-}
neighbors : Position -> CellGrid a -> List a
neighbors position grid =
    let
        x =
            position.row

        y =
            position.column
    in
    List.filterMap (\index_ -> get index_ grid)
        [ { row = x, column = y + 1 }
        , { row = x - 1, column = y + 1 }
        , { row = x - 1, column = y }
        , { row = x - 1, column = y - 1 }
        , { row = x, column = y - 1 }
        , { row = x + 1, column = y - 1 }
        , { row = x + 1, column = y }
        , { row = x + 1, column = y + 1 }
        ]


{-| Get the list of cell values of the four adjacent cells.

    dimensions : Dimensions
    dimensions = Dimensions 3 3

    initializer : Int -> Int -> Int
    initializer i j = arrayIndex dimensions (Position i j)

    cg : CellGrid Int
    cg = CellGrid.initialize dimensions initializer

    CellGrid.adjacent (Position 1 1) cg
        --> [5,1,3,7]

-}
adjacent : Position -> CellGrid a -> List a
adjacent position grid =
    let
        x =
            position.row

        y =
            position.column
    in
    List.filterMap (\index_ -> get index_ grid)
        [ { row = x, column = y + 1 }
        , { row = x - 1, column = y }
        , { row = x, column = y - 1 }
        , { row = x + 1, column = y }
        ]


{-| Convert a `Position` into an index into the flat array.

    arrayIndex (Dimensions 3 2) (Position 0 1) --> 1

    arrayIndex (Dimensions 3 2) (Position 2 1) --> 5

-}
arrayIndex : { a | columns : Int } -> Position -> Int
arrayIndex { columns } position =
    columns * position.row + position.column


{-| Convert an index into the flat array into a `Position`.

    matrixIndex (Dimensions 3 2) 1 --> (Position 0 1)

    matrixIndex (Dimensions 3 2) 5 --> (Position 2 1)

-}
matrixIndex : { a | columns : Int } -> Int -> Position
matrixIndex dimensions n =
    Position (n // dimensions.columns) (modBy dimensions.columns n)


{-| Get a value.

    import Array

    cg : CellGrid Int
    cg = CellGrid (Dimensions 2 2) (Array.fromList [1,2,3,4])

    CellGrid.get (Position 1 1) cg
        --> Just 4

-}
get : Position -> CellGrid a -> Maybe a
get position (CellGrid dimensions array) =
    Array.get (arrayIndex dimensions position) array


{-| Set a value.

    import Array

    cg : CellGrid Int
    cg = CellGrid.repeat (Dimensions 2 2) 42

    CellGrid.set (Position 1 1) 84 cg
        --> CellGrid (Dimensions 2 2) (Array.fromList [42,42,42,84])

-}
set : Position -> a -> CellGrid a -> CellGrid a
set position value (CellGrid dimensions values) =
    let
        k =
            arrayIndex dimensions position
    in
    CellGrid dimensions (Array.set k value values)


{-| Update a value.

    import Array

    cg : CellGrid Int
    cg = CellGrid.repeat (Dimensions 2 2) 42

    CellGrid.update (Position 1 1) (\v -> 2 * v) cg
        --> CellGrid (Dimensions 2 2) (Array.fromList [42,42,42,84])

-}
update : Position -> (a -> a) -> CellGrid a -> CellGrid a
update position updater ((CellGrid dimensions array) as cellGrid) =
    let
        index =
            arrayIndex dimensions position
    in
    case Array.get index array of
        Nothing ->
            cellGrid

        Just value ->
            CellGrid dimensions (Array.set index (updater value) array)


{-| return the type of the cell.

    cg : CellGrid Int
    cg = CellGrid.repeat (Dimensions 3 3) 0

    CellGrid.classifyCell (Position 0 0) cg
        --> Corner

    CellGrid.classifyCell (Position 0 1) cg
        --> Edge

    CellGrid.classifyCell (Position 1 1) cg
        --> Interior

-}
classifyCell : Position -> CellGrid a -> CellType
classifyCell position (CellGrid dimensions _) =
    let
        i =
            position.row

        j =
            position.column

        mri =
            dimensions.rows - 1

        mci =
            dimensions.columns - 1
    in
    if not (i == 0 || j == 0 || i == mri || j == mci) then
        Interior

    else if i == 0 && j == 0 then
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

    grid : CellGrid Bool
    grid =
        CellGrid.repeat (Dimensions 2 3) True

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

    import Array

    CellGrid.initialize (Dimensions 2 2) (\i j -> i)
        --> CellGrid (Dimensions 2 2) (Array.fromList [ 0, 0, 1, 1 ])

    CellGrid.initialize (Dimensions 2 3) (\i j -> toFloat (i + j))
        --> CellGrid (Dimensions 2 3) (Array.fromList [ 0, 1, 2, 1, 2, 3 ])

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

    import Array

    CellGrid.repeat (Dimensions 2 2) 42
        --> CellGrid (Dimensions 2 2) (Array.fromList [ 42, 42, 42, 42 ])

-}
repeat : Dimensions -> a -> CellGrid a
repeat dimensions value =
    let
        n =
            dimensions.rows * dimensions.columns
    in
    CellGrid dimensions (Array.repeat n value)
