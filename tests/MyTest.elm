module MyTest exposing (suite)

import CellGrid exposing (CellGrid, CellType(..), Dimensions, Position)
import Color
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)



-- import TypedSvg.Types exposing (..)


suite : Test
suite =
    describe "The CellGrid module"
        [ describe "Basic functions"
            -- Nest as many descriptions as you like.
            [ test "has valid test data" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList (Dimensions 3 3) (List.range 0 8)
                                |> Maybe.withDefault CellGrid.empty
                    in
                    CellGrid.get (Position 0 0) cg
                        |> Expect.equal (Just 0)
            , test "old vs new implementation of transform" <|
                \_ ->
                    let
                        transform1 : (Int -> Int -> CellGrid a -> a) -> CellGrid a -> CellGrid a
                        transform1 newCellValue aCellGrid =
                            let
                                folder position acc =
                                    CellGrid.set position (newCellValue position.row position.column aCellGrid) acc
                            in
                            List.foldl folder aCellGrid (CellGrid.matrixIndices aCellGrid)

                        cg =
                            CellGrid.fromList (Dimensions 3 3) (List.range 0 8)
                                |> Maybe.withDefault CellGrid.empty

                        old =
                            transform1 (\i j v -> (CellGrid.get (Position i j) v |> Maybe.withDefault 0) + 1) cg

                        new =
                            CellGrid.transform (\i j v -> (CellGrid.get (Position i j) v |> Maybe.withDefault 0) + 1) cg
                    in
                    new
                        |> Expect.equal old
            , test "map" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList (Dimensions 3 3) (List.range 0 8)
                                |> Maybe.withDefault CellGrid.empty

                        cg2 =
                            CellGrid.map String.fromInt cg
                    in
                    Expect.equal (CellGrid.get (Position 0 0) cg2) (Just "0")
            , test "mapWithIndex" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList (Dimensions 3 3) (List.range 0 8)
                                |> Maybe.withDefault CellGrid.empty

                        mapper =
                            \i j v -> v + i + j

                        cg2 =
                            CellGrid.indexedMap mapper cg
                    in
                    Expect.equal (CellGrid.get (Position 2 2) cg2) (Just 12)
            , test "foldl" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList (Dimensions 3 3) (List.range 0 8)
                                |> Maybe.withDefault CellGrid.empty

                        folder =
                            \c acc -> c + acc

                        result =
                            CellGrid.foldl folder 0 cg
                    in
                    Expect.equal result 36
            , test "transform" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList (Dimensions 3 3) (List.range 0 8)
                                |> Maybe.withDefault CellGrid.empty

                        cmAtIndex =
                            \i j g -> CellGrid.get (Position i j) g |> Maybe.withDefault 0

                        transformer =
                            \i j g -> cmAtIndex i j g + i + j

                        cg2 =
                            CellGrid.transform transformer cg
                    in
                    Expect.equal (CellGrid.get (Position 1 1) cg2) (Just 6)
            , test "classifyCell: Corner" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList (Dimensions 4 4) (List.range 0 15)
                                |> Maybe.withDefault CellGrid.empty
                    in
                    Expect.equal (CellGrid.classifyCell (Position 0 0) cg) Corner
            , test "classifyCell: Edge" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList (Dimensions 4 4) (List.range 0 15)
                                |> Maybe.withDefault CellGrid.empty
                    in
                    Expect.equal (CellGrid.classifyCell (Position 2 0) cg) Edge
            , test "classifyCell: Interior" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList (Dimensions 4 4) (List.range 0 15)
                                |> Maybe.withDefault CellGrid.empty
                    in
                    Expect.equal (CellGrid.classifyCell (Position 2 2) cg) Interior
            , test "matrixIndex" <|
                \_ ->
                    let
                        indices =
                            List.map (CellGrid.matrixIndex (Dimensions 2 3)) (List.range 0 5)

                        indices2 =
                            [ Position 0 0, Position 0 1, Position 0 2, Position 1 0, Position 1 1, Position 1 2 ]
                    in
                    Expect.equal indices indices2
            , test "cellAtMatrixIndex" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList (Dimensions 2 3) (List.range 0 5)
                                |> Maybe.withDefault CellGrid.empty

                        value0 =
                            CellGrid.get (Position 0 2) cg

                        value1 =
                            CellGrid.get (Position 1 0) cg

                        value2 =
                            CellGrid.get (Position 1 2) cg
                    in
                    Expect.equal [ value0, value1, value2 ] [ Just 2, Just 3, Just 5 ]
            , test "cellAtMatrixIndex II" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList (Dimensions 2 3) (List.range 0 5)
                                |> Maybe.withDefault CellGrid.empty

                        indices =
                            [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]

                        values =
                            List.map (\( i, j ) -> CellGrid.get (Position i j) cg) indices
                    in
                    Expect.equal values (List.range 0 5 |> List.map Just)
            , test "matrixIndices" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList (Dimensions 2 3) (List.range 0 5)
                                |> Maybe.withDefault CellGrid.empty

                        indices1 =
                            CellGrid.matrixIndices cg

                        indices2 =
                            [ Position 0 0, Position 0 1, Position 0 2, Position 1 0, Position 1 1, Position 1 2 ]
                    in
                    Expect.equal indices1 indices2
            , test "initialize" <|
                \_ ->
                    let
                        cg =
                            CellGrid.initialize (Dimensions 2 3) (\i j -> toFloat (i + j))

                        temperature =
                            CellGrid.get (Position 1 2) cg
                                |> Maybe.withDefault -1
                    in
                    Expect.equal temperature 3.0
            , test "initialize full" <|
                \_ ->
                    let
                        cg =
                            CellGrid.initialize (Dimensions 2 3) (\i j -> toFloat (i + j))
                    in
                    cg
                        |> Just
                        |> Expect.equal (CellGrid.fromList (Dimensions 2 3) [ 0, 1, 2, 1, 2, 3 ])
            , test "repeat" <|
                \_ ->
                    let
                        cg =
                            CellGrid.repeat (Dimensions 2 3) 42
                    in
                    cg
                        |> Just
                        |> Expect.equal (CellGrid.fromList (Dimensions 2 3) (List.repeat (2 * 3) 42))
            , test "adjacent" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList (Dimensions 4 4) (List.range 0 15)
                                |> Maybe.withDefault CellGrid.empty

                        expectedAdjacent =
                            [ 6, 1, 4, 9 ]
                    in
                    Expect.equal (CellGrid.adjacent (Position 1 1) cg) expectedAdjacent
            , test "neighbors" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList (Dimensions 4 4) (List.range 0 15)
                                |> Maybe.withDefault CellGrid.empty

                        expectedNeighbors =
                            [ 6, 2, 1, 0, 4, 8, 9, 10 ]
                    in
                    Expect.equal (CellGrid.neighbors (Position 1 1) cg) expectedNeighbors
            ]
        ]



-- transform : (( Int, Int ) -> CellGrid a -> b) -> CellGrid a -> CellGrid b
