module HeatKernelBenchmark exposing (suite)

import Benchmark exposing (..)
import CellGrid
import CellGrid.Render exposing (CellRenderer)
import CellGrid.WebGL
import Color exposing (Color)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import TemperatureField


n =
    80


gridWidth =
    20


gridDisplayWidth =
    500


colorMap : Float -> Vec3
colorMap t =
    vec3 t 0 0


cellrenderer : CellRenderer Float
cellrenderer =
    { cellSize = gridDisplayWidth / toFloat gridWidth
    , cellColorizer = \z -> Color.rgb z 0 0
    , defaultColor = Color.rgb 0 0 0
    , gridLineColor = Color.rgb 180 0 0
    , gridLineWidth = 0.5
    }


suite : Benchmark
suite =
    let
        tf =
            TemperatureField.randomHeatMap ( n, n )
    in
    describe "temperature field"
        [ -- nest as many descriptions as you like
          -- describe "update"
          --   [ benchmark "temperature field" <|
          --       \_ -> TemperatureField.updateCells 0.01 tf
          --   ]
          -- describe "render (SVG)"
          --   [ benchmark "temperature field" <|
          --       \_ -> CellGrid.Render.asHtml 400 400 cellrenderer tf
          --   ]
          describe "render (WebGL)"
            [ benchmark "temperature field" <|
                \_ -> CellGrid.WebGL.asHtml 500 500 tf colorMap
            ]
        ]
