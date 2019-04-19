module HeatMapBenchmarks exposing (..)

import Benchmark exposing(..)
import HeatMap exposing(..)
import Array


suite : Benchmark
suite =
    let
         heatMap = randomHeatMap (8,8)
    in
    describe "HeatMap"
        [ -- nest as many descriptions as you like
          describe "update"
            [ benchmark "Run update on 5x5 array" <|
                \_ -> updateCells 0.5 heatMap


            ]
        ]