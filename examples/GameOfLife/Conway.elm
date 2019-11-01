module Conway exposing (State(..), randomCellGrid, updateCells, spot, occupied, occupy, toggleState)

import Array exposing (Array)
import Random
import Maybe.Extra
import CellGrid exposing (CellGrid(..), Dimensions, Position,  CellType(..) )

type State = Occupied | Unoccupied

updateCells : CellGrid State -> CellGrid State
updateCells cellGrid =
    CellGrid.transform nextValue cellGrid


randomCellGrid : Int -> Float -> ( Int, Int ) -> CellGrid State
randomCellGrid seed density ( r, c ) =
    CellGrid {rows = r, columns = c } (Array.fromList <| cellSequence density (r * c) seed ( 0, 1 ))


occupy :  (Int, Int) -> CellGrid State -> CellGrid State
occupy (i, j) cg  =
    CellGrid.set (Position i j ) Occupied cg

toggleState :  (Int, Int) -> CellGrid State -> CellGrid State
toggleState (i, j) cg  =
    case CellGrid.get (Position i j) cg of
        Nothing -> cg
        Just state ->
            case state of
                Unoccupied -> CellGrid.set (Position i j ) Occupied cg
                Occupied -> CellGrid.set (Position i j ) Unoccupied cg

spot : (Int, Int) -> Float -> State -> CellGrid State -> CellGrid State
spot (centerI, centerJ) radius state cg =
    let
        cellTransformer : Int -> Int -> State -> State
        cellTransformer i j t =
            let
                di = toFloat <| i - centerI
                dj = toFloat <| j - centerJ
            in
            case di*di + dj*dj <= radius*radius of
                True -> state
                False -> t
     in
     CellGrid.indexedMap cellTransformer cg


cellSequence : Float -> Int -> Int -> ( Float, Float ) -> List State
cellSequence density n seed ( a, b ) =
    cellSequence_ n (makeSeed seed) ( a, b )
        |> Tuple.first
        |> List.map (chooseState density)

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


nextValue : Int -> Int  -> CellGrid State -> State
nextValue i j cellGrid =
    case CellGrid.get  (Position i j) cellGrid of
        Nothing -> Unoccupied
        Just state ->
            let
                nOccupied = occupiedNeighbors cellGrid (i,j)
            in
            case (state, nOccupied) of
                (Unoccupied, 3) -> Occupied
                (Unoccupied, _) -> Unoccupied
                (Occupied, 2) -> Occupied
                (Occupied, 3) -> Occupied
                (Occupied, _) -> Unoccupied


neighborFilter : CellGrid u -> ( Int, Int ) -> Bool
neighborFilter (CellGrid dimensions _) ( a, b ) =
    a >= 0 && a < dimensions.rows && b >= 0 && b < dimensions.columns


neighborIndices : CellGrid a -> (Int, Int) -> List ( Int, Int )
neighborIndices cg (row, col) =
    [ ( row - 1, col ), ( row + 1, col ), ( row, col - 1 ), ( row, col + 1 ),
       (row - 1, col - 1), (row - 1, col + 1), (row + 1, col - 1), (row + 1, col + 1)]
        |> List.filter (neighborFilter cg)


neighbors : CellGrid a -> (Int, Int) -> List a
neighbors cg (row, col)  =
    neighborIndices cg (row, col)
        |> List.map (\( r, c ) -> CellGrid.get (Position r c) cg)
        |> Maybe.Extra.values

occupied : CellGrid State  -> Int
occupied (CellGrid _ cells) =
     cells
       |> Array.filter (\state -> state == Occupied)
       |> Array.length

occupiedNeighbors : CellGrid State -> (Int, Int) -> Int
occupiedNeighbors cg (row, col) =
    neighbors cg (row, col)
      |> List.filter (\state -> state == Occupied)
      |> List.length

