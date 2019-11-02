module TestRenderWebGL exposing (suite)

import CellGrid exposing (CellGrid, CellType(..), Dimensions, Position, matrixIndices)
import CellGrid.RenderWebGL
import Color exposing (Color)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Math.Vector3 as Vec3
import Test exposing (..)
import WebGL exposing (Mesh)


suite =
    describe "render webgl"
        [ describe "meshWithColorizer"
            [ test "2x2 0x0" <|
                \_ ->
                    meshWithColorizerNew (Dimensions 2 2) { cellHeight = 0, cellWidth = 0, toColor = \_ -> Color.black }
                        |> Expect.equal
                            [ ( { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 } )
                            , ( { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 } )
                            , ( { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 } )
                            , ( { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 } )
                            , ( { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 } )
                            , ( { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 } )
                            , ( { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 } )
                            , ( { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 } )
                            ]
            , test "2x2 1x1" <|
                \_ ->
                    meshWithColorizerNew (Dimensions 2 2) { cellHeight = 1, cellWidth = 1, toColor = \_ -> Color.black }
                        |> Expect.equal
                            [ ( { b = 0, g = 0, r = 0, x = -1, y = 1, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = 1, z = 0 }, { b = 0, g = 0, r = 0, x = -1, y = 0, z = 0 } )
                            , ( { b = 0, g = 0, r = 0, x = 0, y = 1, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = -1, y = 0, z = 0 } )
                            , ( { b = 0, g = 0, r = 0, x = 0, y = 1, z = 0 }, { b = 0, g = 0, r = 0, x = 1, y = 1, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = 0, z = 0 } )
                            , ( { b = 0, g = 0, r = 0, x = 1, y = 1, z = 0 }, { b = 0, g = 0, r = 0, x = 1, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = 0, z = 0 } )
                            , ( { b = 0, g = 0, r = 0, x = -1, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = -1, y = -1, z = 0 } )
                            , ( { b = 0, g = 0, r = 0, x = 0, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = -1, z = 0 }, { b = 0, g = 0, r = 0, x = -1, y = -1, z = 0 } )
                            , ( { b = 0, g = 0, r = 0, x = 0, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = 1, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = -1, z = 0 } )
                            , ( { b = 0, g = 0, r = 0, x = 1, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = 1, y = -1, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = -1, z = 0 } )
                            ]
            ]
        , describe "meshFromCellGrid" <|
            case CellGrid.fromList (Dimensions 2 2) [ 1.0, 2.0, 3.0, 4.0 ] of
                Just cellGrid ->
                    [ test "2x2 1x1" <|
                        \_ ->
                            CellGrid.RenderWebGL.meshFromCellGridHelp { cellWidth = 1, cellHeight = 1, toColor = \_ -> Color.black } cellGrid
                                |> Expect.equal
                                    [ ( { b = 0, g = 0, r = 0, x = -1, y = 1, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = 1, z = 0 }, { b = 0, g = 0, r = 0, x = -1, y = 0, z = 0 } )
                                    , ( { b = 0, g = 0, r = 0, x = 0, y = 1, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = -1, y = 0, z = 0 } )
                                    , ( { b = 0, g = 0, r = 0, x = 0, y = 1, z = 0 }, { b = 0, g = 0, r = 0, x = 1, y = 1, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = 0, z = 0 } )
                                    , ( { b = 0, g = 0, r = 0, x = 1, y = 1, z = 0 }, { b = 0, g = 0, r = 0, x = 1, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = 0, z = 0 } )
                                    , ( { b = 0, g = 0, r = 0, x = -1, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = -1, y = -1, z = 0 } )
                                    , ( { b = 0, g = 0, r = 0, x = 0, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = -1, z = 0 }, { b = 0, g = 0, r = 0, x = -1, y = -1, z = 0 } )
                                    , ( { b = 0, g = 0, r = 0, x = 0, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = 1, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = -1, z = 0 } )
                                    , ( { b = 0, g = 0, r = 0, x = 1, y = 0, z = 0 }, { b = 0, g = 0, r = 0, x = 1, y = -1, z = 0 }, { b = 0, g = 0, r = 0, x = 0, y = -1, z = 0 } )
                                    ]
                    ]

                Nothing ->
                    []
        ]


meshWithColorizerNew :
    Dimensions
    -> CellGrid.RenderWebGL.CellStyle Position
    -> List ( CellGrid.RenderWebGL.Vertex, CellGrid.RenderWebGL.Vertex, CellGrid.RenderWebGL.Vertex )
meshWithColorizerNew size style =
    CellGrid.initialize size (\i j -> style.toColor (Position i j))
        |> CellGrid.RenderWebGL.meshFromCellGridHelp { cellWidth = style.cellWidth, cellHeight = style.cellHeight, toColor = identity }
