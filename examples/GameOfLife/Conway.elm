module Conway exposing (State(..), randomCellGrid, updateCells, spot)

import Array exposing (Array)
import Random
import Maybe.Extra
import CellGrid exposing (CellGrid(..), cellAtIndex, classifyCell, CellType(..), setValue, indices)

type State = Occupied | Unoccupied

updateCells : CellGrid State -> CellGrid State
updateCells cellGrid =
    List.foldl (\( i, j ) acc -> setValue acc ( i, j ) (nextValue ( i, j ) cellGrid)) cellGrid (indices cellGrid)



randomCellGrid : ( Int, Int ) -> CellGrid State
randomCellGrid ( r, c ) =
    CellGrid ( r, c ) (Array.fromList <| cellSequence (r * c) 0 ( 0, 1 ))


spot : (Int, Int) -> Float -> State -> CellGrid State -> CellGrid State
spot (centerI, centerJ) radius state cg =
    let
        cellTransformer : (Int, Int) -> State -> State
        cellTransformer (i, j) t =
            let
                di = toFloat <| i - centerI
                dj = toFloat <| j - centerJ
            in
            case di*di + dj*dj <= radius*radius of
                True -> state
                False -> t
     in
     CellGrid.mapWithIndex cellTransformer cg


cellSequence : Int -> Int -> ( Float, Float ) -> List State
cellSequence n k ( a, b ) =
    cellSequence_ n (makeSeed k) ( a, b )
        |> Tuple.first
        |> List.map (chooseState 0.30)

chooseState : Float -> Float -> State
chooseState p rn =
    if rn < p then
      Occupied
    else
      Unoccupied


gen : Int -> ( Float, Float ) -> Random.Generator (List Float)
gen n ( a, b ) =
    Random.list n (Random.float a b)


makeSeed : Int -> Random.Seed
makeSeed k =
    Random.initialSeed k


cellSequence_ : Int -> Random.Seed -> ( Float, Float ) -> ( List Float, Random.Seed )
cellSequence_ n seed ( a, b ) =
    Random.step (gen n ( a, b )) seed



nextValue : ( Int, Int ) -> CellGrid State -> State
nextValue ( i, j ) cellGrid =
    case cellAtIndex (i, j) cellGrid of
        Nothing -> Unoccupied
        Just state ->
            let
                nOccupied = occupied cellGrid (i,j)
            in
            case (state, nOccupied) of
                (Unoccupied, 3) -> Occupied
                (Unoccupied, _) -> Unoccupied
                (Occupied, 2) -> Occupied
                (Occupied, 3) -> Occupied
                (Occupied, _) -> Unoccupied



neighborFilter : CellGrid u -> ( Int, Int ) -> Bool
neighborFilter (CellGrid (nRows, nCols) _) ( a, b ) =
    a >= 0 && a < nRows && b >= 0 && b < nCols


neighborIndices : CellGrid a -> (Int, Int) -> List ( Int, Int )
neighborIndices cg (row, col) =
    [ ( row - 1, col ), ( row + 1, col ), ( row, col - 1 ), ( row, col + 1 ),
       (row - 1, col - 1), (row - 1, col + 1), (row + 1, col - 1), (row + 1, col - 1)]
        |> List.filter (neighborFilter cg)


neighbors : CellGrid a -> (Int, Int) -> List a
neighbors cg (row, col)  =
    neighborIndices cg (row, col)
        |> List.map (\( r, c ) -> cellAtIndex (r, c) cg)
        |> Maybe.Extra.values

occupied : CellGrid State -> (Int, Int) -> Int
occupied cg (row, col) =
    neighbors cg (row, col)
      |> List.filter (\state -> state == Occupied)
      |> List.length

updateCell : ( Int, Int ) -> CellGrid State -> CellGrid State
updateCell ( i, j ) cellGrid =
    setValue cellGrid ( i, j ) (nextValue ( i, j ) cellGrid)