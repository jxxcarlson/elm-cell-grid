module TemperatureField exposing (randomHeatMap, updateCells, spot)

import Array exposing (Array)
import Random
import CellGrid exposing (CellGrid(..), cellAtMatrixIndex, classifyCell, CellType(..))


updateCells : Float -> CellGrid Float -> CellGrid Float
updateCells beta temperatureField =
    CellGrid.transform (nextCellValue beta) temperatureField


averageAt : CellGrid Float -> ( Int, Int ) -> Float
averageAt temperatureField ( i, j ) =
    let
        east =
            cellAtMatrixIndex ( i - 1, j ) temperatureField |> Maybe.withDefault 0

        west =
            cellAtMatrixIndex ( i + 1, j ) temperatureField |> Maybe.withDefault 0

        north =
            cellAtMatrixIndex ( i, j + 1 ) temperatureField |> Maybe.withDefault 0

        south =
            cellAtMatrixIndex ( i, j - 1 ) temperatureField |> Maybe.withDefault 0

        denominator =
            case classifyCell temperatureField ( i, j ) of
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
spot (centerI, centerJ) radius temperature temperatureField =
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
     CellGrid.mapWithIndex cellTransformer temperatureField


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
nextCellValue beta ( i, j ) temperatureField =
    let
        currentCellValue =
            cellAtMatrixIndex ( i, j ) temperatureField |> Maybe.withDefault 0
    in
        case classifyCell temperatureField ( i, j ) == Interior of
            False ->
                currentCellValue

            True ->
                (1 - beta) * currentCellValue + beta * (averageAt temperatureField ( i, j ))

