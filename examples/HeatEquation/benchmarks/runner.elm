module Main exposing (main)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import HeatKernelBenchmark exposing (..)


main : BenchmarkProgram
main =
    program suite
