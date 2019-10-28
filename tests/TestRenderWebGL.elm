module TestRenderWebGL exposing (suite)

import CellGrid exposing (CellGrid, CellType(..), matrixIndices, setValue)
import CellGrid.RenderWebGL
import Color
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Math.Vector3 as Vec3
import Test exposing (..)
import WebGL


suite =
    describe "render webgl"
        [ describe "meshWithColorizer"
            [ test "2x2 0x0" <|
                \_ ->
                    CellGrid.RenderWebGL.meshWithColorizerHelp (\_ -> Vec3.vec3 0 0 0) ( 2, 2 ) ( 0, 0 )
                        |> Expect.equal
                            [ ( { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 } )
                            , ( { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 } )
                            , ( { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 } )
                            , ( { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 } )
                            , ( { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 } )
                            , ( { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 } )
                            , ( { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 } )
                            , ( { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 } )
                            ]
            , test "2x2 1x1" <|
                \_ ->
                    CellGrid.RenderWebGL.meshWithColorizerHelp (\_ -> Vec3.vec3 0 0 0) ( 2, 2 ) ( 1, 1 )
                        |> Expect.equal
                            [ ( { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 0, z = 0 } )
                            , ( { color = Vec3.vec3 0 0 0, x = 0, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 0, z = 0 } )
                            , ( { color = Vec3.vec3 0 0 0, x = -1, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = -1, z = 0 } )
                            , ( { color = Vec3.vec3 0 0 0, x = 0, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = -1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = -1, z = 0 } )
                            , ( { color = Vec3.vec3 0 0 0, x = 0, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = 1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = 0, z = 0 } )
                            , ( { color = Vec3.vec3 0 0 0, x = 1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = 1, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = 0, z = 0 } )
                            , ( { color = Vec3.vec3 0 0 0, x = 0, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = 1, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = -1, z = 0 } )
                            , ( { color = Vec3.vec3 0 0 0, x = 1, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = 1, y = -1, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = -1, z = 0 } )
                            ]
            ]
        , describe "meshFromCellGrid" <|
            case CellGrid.fromList 2 2 [ 1.0, 2.0, 3.0, 4.0 ] of
                Just cellGrid ->
                    [ test "2x2 1x1" <|
                        \_ ->
                            CellGrid.RenderWebGL.meshFromCellGridHelp ( 1, 1 ) (\_ -> Vec3.vec3 0 0 0) cellGrid
                                |> Expect.equal
                                    [ ( { color = Vec3.vec3 0 0 0, x = -1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 0, z = 0 } )
                                    , ( { color = Vec3.vec3 0 0 0, x = 0, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = 0, z = 0 } )
                                    , ( { color = Vec3.vec3 0 0 0, x = -1, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = -1, z = 0 } )
                                    , ( { color = Vec3.vec3 0 0 0, x = 0, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = -1, z = 0 }, { color = Vec3.vec3 0 0 0, x = -1, y = -1, z = 0 } )
                                    , ( { color = Vec3.vec3 0 0 0, x = 0, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = 1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = 0, z = 0 } )
                                    , ( { color = Vec3.vec3 0 0 0, x = 1, y = 1, z = 0 }, { color = Vec3.vec3 0 0 0, x = 1, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = 0, z = 0 } )
                                    , ( { color = Vec3.vec3 0 0 0, x = 0, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = 1, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = -1, z = 0 } )
                                    , ( { color = Vec3.vec3 0 0 0, x = 1, y = 0, z = 0 }, { color = Vec3.vec3 0 0 0, x = 1, y = -1, z = 0 }, { color = Vec3.vec3 0 0 0, x = 0, y = -1, z = 0 } )
                                    ]
                    ]

                Nothing ->
                    []
        ]
