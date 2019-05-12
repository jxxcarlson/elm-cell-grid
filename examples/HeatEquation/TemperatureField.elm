module TemperatureField exposing (randomHeatMap, updateCells, spot)

import Array exposing (Array)
import Random
import CellGrid exposing (CellGrid(..), cellAtMatrixIndex, classifyCell, CellType(..), setValue, matrixIndices)


updateCells : Float -> CellGrid Float -> CellGrid Float
updateCells beta heatMap =
    CellGrid.transform (nextCellValue beta) heatMap


averageAt : CellGrid Float -> ( Int, Int ) -> Float
averageAt heatMap ( i, j ) =
    let
        east =
            cellAtMatrixIndex ( i - 1, j ) heatMap |> Maybe.withDefault 0

        west =
            cellAtMatrixIndex ( i + 1, j ) heatMap |> Maybe.withDefault 0

        north =
            cellAtMatrixIndex ( i, j + 1 ) heatMap |> Maybe.withDefault 0

        south =
            cellAtMatrixIndex ( i, j - 1 ) heatMap |> Maybe.withDefault 0

        denominator =
            case classifyCell heatMap ( i, j ) of
                Interior ->
                    4

                Edge ->
                    3
                Corner ->
                    2
    in
        (east + west + north + south) / denominator


randomHeatMap : ( Int, Int ) -> CellGrid Float
randomHeatMap ( r, c ) =
    CellGrid ( r, c ) (Array.fromList <| floatSequence (r * c) 0 ( 0, 1 ))


spot : (Int, Int) -> Float -> Float -> CellGrid Float -> CellGrid Float
spot (centerI, centerJ) radius temperature heatMap =
    let
        cellTransformer : (Int, Int) -> Float -> Float
        cellTransformer (i, j) t =
            let
                di = toFloat <| i - centerI
                dj = toFloat <| j - centerJ
            in
            case di*di + dj*dj <= radius*radius of
                True -> temperature
                False -> t
     in
     CellGrid.mapWithIndex cellTransformer heatMap


floatSequence : Int -> Int -> ( Float, Float ) -> List Float
floatSequence n k ( a, b ) =
    floatSequence_ n (makeSeed k) ( a, b )
        |> Tuple.first


gen : Int -> ( Float, Float ) -> Random.Generator (List Float)
gen n ( a, b ) =
    Random.list n (Random.float a b)


makeSeed : Int -> Random.Seed
makeSeed k =
    Random.initialSeed k


floatSequence_ : Int -> Random.Seed -> ( Float, Float ) -> ( List Float, Random.Seed )
floatSequence_ n seed ( a, b ) =
    Random.step (gen n ( a, b )) seed



nextCellValue : Float -> ( Int, Int ) -> CellGrid Float -> Float
nextCellValue beta ( i, j ) heatMap =
    let
        currentCellValue =
            cellAtMatrixIndex ( i, j ) heatMap |> Maybe.withDefault 0
    in
        case classifyCell heatMap ( i, j ) == Interior of
            False ->
                currentCellValue

            True ->
                (1 - beta) * currentCellValue + beta * (averageAt heatMap ( i, j ))


updateCell : Float -> ( Int, Int ) -> CellGrid Float -> CellGrid Float
updateCell beta ( i, j ) heatMap =
    setValue heatMap ( i, j ) (nextCellValue beta ( i, j ) heatMap)