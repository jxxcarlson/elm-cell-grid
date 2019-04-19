import Benchmark.Runner exposing (BenchmarkProgram, program)
import HeatMapBenchmarks exposing(suite)

main : BenchmarkProgram
main =
    program suite