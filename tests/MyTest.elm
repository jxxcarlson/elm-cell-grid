module MyTest exposing (suite)

import CellGrid exposing (CellGrid, CellType(..), matrixIndices, setValue)
import CellGrid.WebGL
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
                            CellGrid.fromList 3 3 (List.range 0 8)
                                |> Maybe.withDefault CellGrid.empty
                    in
                    Expect.equal (CellGrid.cellAtMatrixIndex ( 0, 0 ) cg) (Just 0)
            , test "old vs new implementation of transform" <|
                \_ ->
                    let
                        transform1 : (( Int, Int ) -> CellGrid a -> a) -> CellGrid a -> CellGrid a
                        transform1 newCellValue aCellGrid =
                            List.foldl (\( i, j ) acc -> setValue acc ( i, j ) (newCellValue ( i, j ) aCellGrid)) aCellGrid (matrixIndices aCellGrid)

                        cg =
                            CellGrid.fromList 3 3 (List.range 0 8)
                                |> Maybe.withDefault CellGrid.empty

                        old =
                            transform1 (\( i, j ) v -> (CellGrid.cellAtMatrixIndex ( i, j ) v |> Maybe.withDefault 0) + 1) cg

                        new =
                            CellGrid.transform (\( i, j ) v -> (CellGrid.cellAtMatrixIndex ( i, j ) v |> Maybe.withDefault 0) + 1) cg
                    in
                    new
                        |> Expect.equal old
            , test "map" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList 3 3 (List.range 0 8)
                                |> Maybe.withDefault CellGrid.empty

                        cg2 =
                            CellGrid.map String.fromInt cg
                    in
                    Expect.equal (CellGrid.cellAtMatrixIndex ( 0, 0 ) cg2) (Just "0")
            , test "mapWithIndex" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList 3 3 (List.range 0 8)
                                |> Maybe.withDefault CellGrid.empty

                        mapper =
                            \( i, j ) v -> v + i + j

                        cg2 =
                            CellGrid.mapWithIndex mapper cg
                    in
                    Expect.equal (CellGrid.cellAtMatrixIndex ( 2, 2 ) cg2) (Just 12)
            , test "foldl" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList 3 3 (List.range 0 8)
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
                            CellGrid.fromList 3 3 (List.range 0 8)
                                |> Maybe.withDefault CellGrid.empty

                        cmAtIndex =
                            \( i, j ) g -> CellGrid.cellAtMatrixIndex ( i, j ) g |> Maybe.withDefault 0

                        transformer =
                            \( i, j ) g -> cmAtIndex ( i, j ) g + i + j

                        cg2 =
                            CellGrid.transform transformer cg
                    in
                    Expect.equal (CellGrid.cellAtMatrixIndex ( 1, 1 ) cg2) (Just 6)
            , test "classifyCell: Corner" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList 4 4 (List.range 0 8)
                                |> Maybe.withDefault CellGrid.empty
                    in
                    Expect.equal (CellGrid.classifyCell ( 0, 0 ) cg) Corner
            , test "classifyCell: Edge" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList 4 4 (List.range 0 8)
                                |> Maybe.withDefault CellGrid.empty
                    in
                    Expect.equal (CellGrid.classifyCell ( 2, 0 ) cg) Edge
            , test "classifyCell: Interior" <|
                \_ ->
                    let
                        cg =
                            CellGrid.fromList 4 4 (List.range 0 8)
                                |> Maybe.withDefault CellGrid.empty
                    in
                    Expect.equal (CellGrid.classifyCell ( 2, 2 ) cg) Interior
            ]
        ]



-- transform : (( Int, Int ) -> CellGrid a -> b) -> CellGrid a -> CellGrid b
