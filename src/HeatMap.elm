module HeatMap exposing (HeatMap(..), location, index, cellAtIndex, setValue, nextCellValue, updateCell, averageAt, randomHeatMap)

{-| This library is just a test.  I repeat: a test!

@docs location, index

-}

import Array exposing(Array)
import Random
import List.Extra

type HeatMap = HeatMap (Int, Int) (Array Float)

rows : HeatMap  -> Int
rows (HeatMap (rows_, _) _) = rows_

cols : HeatMap              -> Int
cols (HeatMap (r_, cols_) _) = cols_

dimensions : HeatMap -> (Int, Int)
dimensions (HeatMap idx _) =
    idx

location : Int -> (Int,  Int) -> Int
location nRows (row, col) =
    nRows * row + col

index : (Int, Int) -> Int -> (Int, Int)
index (nRows, nCols)  n =
    (n // nCols, modBy nRows n)

cellAtIndex: (Int,Int) -> HeatMap -> Float
cellAtIndex (i,j) heatMap =
    let
        (HeatMap (nRows, _) array) = heatMap
    in
       Array.get (location nRows (i, j)) array
         |> Maybe.withDefault 0


setValue : HeatMap -> (Int,Int) -> Float -> HeatMap
setValue (HeatMap (nRows, nCols) values) (i,j) value =
    let
       k = location nRows (i,j)
    in
       (HeatMap (nRows, nCols) (Array.set k value values))

type CellType = Corner | Edge | Interior

classifyCell : HeatMap -> (Int, Int) -> CellType
classifyCell heatMap (i,j) =
    let
        (nRows, nCols) = dimensions heatMap
        mri = nRows - 1
        mci = nCols - 1
     in
     case i == 0 || j == 0 || i == mri || j == mci of
         False -> Interior
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






averageAt : HeatMap -> (Int,Int) -> Float
averageAt heatMap (i,j) =
    let
        east = cellAtIndex (i - 1,j) heatMap
        west = cellAtIndex (i + 1,j) heatMap
        north = cellAtIndex (i,j + 1) heatMap
        south = cellAtIndex (i,j - 1 ) heatMap
        denominator =
            case classifyCell heatMap (i,j) of
                Interior -> 4
                Edge -> 3
                Corner -> 2
    in
       (east + west + north + south)/denominator



randomHeatMap : (Int, Int) -> HeatMap
randomHeatMap (r,c) =
       HeatMap (r,c) (Array.fromList <| floatSequence (r * c) 0 (0,1))


nextCellValue : Float ->(Int, Int) -> HeatMap ->  Float
nextCellValue beta (i,j) heatMap  =
    let
        currentCellValue = cellAtIndex (i,j) heatMap
    in
        (1 - beta) * currentCellValue + beta * (averageAt heatMap (i,j))


updateCell : Float -> (Int, Int) -> HeatMap ->  HeatMap
updateCell beta (i,j) heatMap  =
    setValue heatMap (i,j) (nextCellValue beta (i,j) heatMap)




---
--- RNG
---


{-

Example:

> RNG.floatSequence 3 23 (0,1)
[0.07049563320325747,0.8633668118636881,0.6762363032990798]

-}

floatSequence : Int -> Int -> (Float, Float) -> List Float
floatSequence n k (a,b) =
    floatSequence_ n (makeSeed k) (a,b)
      |> Tuple.first

gen : Int -> (Float, Float) -> Random.Generator (List Float)
gen n  (a, b) =
    Random.list n (Random.float a b)

makeSeed : Int -> Random.Seed
makeSeed k =
    Random.initialSeed k

floatSequence_ : Int -> Random.Seed -> (Float, Float) -> (List Float, Random.Seed)
floatSequence_ n seed (a,b) =
    Random.step (gen n (a,b)) seed