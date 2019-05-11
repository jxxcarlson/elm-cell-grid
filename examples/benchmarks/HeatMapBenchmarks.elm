module HeatMapBenchmarks exposing (..)

import Benchmark exposing(..)
import CellGrid exposing(..)
import Array


suite : Benchmark
suite =
    let
         heatMap = randomHeatMap (8,8)
    in
    describe "CellGrid"
        [ -- nest as many descriptions as you like
          describe "update"
            [ benchmark "Run update on 5x5 array" <|
                \_ -> updateCells 0.5 heatMap


            ]
        ]