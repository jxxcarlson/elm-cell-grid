module Example exposing (..)

import Expect exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import HeatMap exposing(..)
import Array exposing(Array)

suite : Test
suite =
    let
        cellList = List.range 0 10 |> List.map toFloat
        heatMap = HeatMap (3,3) (Array.fromList cellList)
        epsilon = 0.0000001

    in
    describe "The HeatMap module"
            [ describe "Access functions" -- Nest as many descriptions as you like.
                [ test "cellAtIndex"  <|
                    \_ ->
                        let
                            c = HeatMap.cellAtIndex (2,2) heatMap
                        in
                            Expect.equal c 8.0

                , test "average temperature at an interior cell" <|
                    \_ ->
                        averageAt heatMap (1,1)  |> Expect.within (Absolute epsilon) 4.0

                , test "average temperature at an edge cell" <|
                    \_ ->
                        averageAt heatMap (0,1)  |> Expect.within (Absolute epsilon) 2.0

                , test "average temperature at a corner cell" <|
                    \_ ->
                        averageAt heatMap (0,0)  |> Expect.within (Absolute epsilon) 2.0

                , test "set cell value" <|
                    \_ ->
                        setValue heatMap (1,1) 0 |> cellAtIndex (1,1) |> Expect.within (Absolute epsilon) 0.0

                , test "compute next temperature value" <|
                    \_ ->
                        setValue heatMap (1,1) 0
                          |> nextCellValue 0.5 (1,1)
                          |> Expect.within (Absolute epsilon) 2.0

                , test "update array with next temperature value" <|
                    \_ ->
                        setValue heatMap (1,1) 0
                          |> updateCell 0.5 (1,1)
                          |> cellAtIndex (1,1)
                          |> Expect.within (Absolute epsilon) 2.0


              ]
            ]
