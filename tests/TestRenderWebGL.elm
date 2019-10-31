module TestRenderWebGL exposing (suite)

import CellGrid exposing (CellGrid, CellType(..), matrixIndices, setValue)
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
                    CellGrid.RenderWebGL.meshWithColorizerHelp (\_ -> Color.black) ( 2, 2 ) { width = 0, height = 0 }
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
                    CellGrid.RenderWebGL.meshWithColorizerHelp (\_ -> Color.black) ( 2, 2 ) { width = 1, height = 1 }
                        |> Expect.equal
                            [ ( { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 0, z = 0 } )
                            , ( { r = 0, g = 0, b = 0, x = 0, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 0, z = 0 } )
                            , ( { r = 0, g = 0, b = 0, x = -1, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = -1, z = 0 } )
                            , ( { r = 0, g = 0, b = 0, x = 0, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = -1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = -1, z = 0 } )
                            , ( { r = 0, g = 0, b = 0, x = 0, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = 1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = 0, z = 0 } )
                            , ( { r = 0, g = 0, b = 0, x = 1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = 1, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = 0, z = 0 } )
                            , ( { r = 0, g = 0, b = 0, x = 0, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = 1, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = -1, z = 0 } )
                            , ( { r = 0, g = 0, b = 0, x = 1, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = 1, y = -1, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = -1, z = 0 } )
                            ]
            , test "new implementation is equivalent" <|
                \_ ->
                    let
                        old =
                            CellGrid.RenderWebGL.meshWithColorizerHelp (\_ -> Color.black) ( 2, 2 ) { width = 1, height = 1 }

                        new =
                            meshWithColorizerNew (\_ -> Color.black) ( 2, 2 ) { width = 1, height = 1 }
                    in
                    new |> Expect.equal old
            ]
        , describe "meshFromCellGrid" <|
            case CellGrid.fromList 2 2 [ 1.0, 2.0, 3.0, 4.0 ] of
                Just cellGrid ->
                    [ test "2x2 1x1" <|
                        \_ ->
                            CellGrid.RenderWebGL.meshFromCellGridHelp { width = 1, height = 1 } (\_ -> Color.black) cellGrid
                                |> Expect.equal
                                    [ ( { r = 0, g = 0, b = 0, x = -1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 0, z = 0 } )
                                    , ( { r = 0, g = 0, b = 0, x = 0, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = 0, z = 0 } )
                                    , ( { r = 0, g = 0, b = 0, x = -1, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = -1, z = 0 } )
                                    , ( { r = 0, g = 0, b = 0, x = 0, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = -1, z = 0 }, { r = 0, g = 0, b = 0, x = -1, y = -1, z = 0 } )
                                    , ( { r = 0, g = 0, b = 0, x = 0, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = 1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = 0, z = 0 } )
                                    , ( { r = 0, g = 0, b = 0, x = 1, y = 1, z = 0 }, { r = 0, g = 0, b = 0, x = 1, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = 0, z = 0 } )
                                    , ( { r = 0, g = 0, b = 0, x = 0, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = 1, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = -1, z = 0 } )
                                    , ( { r = 0, g = 0, b = 0, x = 1, y = 0, z = 0 }, { r = 0, g = 0, b = 0, x = 1, y = -1, z = 0 }, { r = 0, g = 0, b = 0, x = 0, y = -1, z = 0 } )
                                    ]
                    ]

                Nothing ->
                    []
        ]


meshWithColorizerNew :
    (( Int, Int ) -> Color)
    -> ( Int, Int )
    ->
        { width : Float
        , height : Float
        }
    -> List ( CellGrid.RenderWebGL.Vertex, CellGrid.RenderWebGL.Vertex, CellGrid.RenderWebGL.Vertex )
meshWithColorizerNew toColor size rectangle =
    CellGrid.initialize size toColor
        |> CellGrid.RenderWebGL.meshFromCellGridHelp rectangle identity
