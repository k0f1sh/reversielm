module Tests where

import ElmTest exposing (..)

import String
import Reversi exposing (..)

all : Test
all =
    suite "A Test Suite"
        [
         test "Addition" (assertEqual (3 + 7) 10)
        ,test "Reverse Black" (assertEqual White (reverse Black))
        ,test "Reverse White" (assertEqual Black (reverse White))
        ,test "Reverse None" (assertEqual None (reverse None))
        ,test "combinationOf 1" (assertEqual 64 (List.length (combinationOf (\a -> \b -> {piece = a, pos = b}) [1..8] [1..8])))
        ,test "directions" (assertEqual 8 (combinationOf (\d1 -> \d2 -> (d1, d2)) [-1, 0, 1] [-1, 0, 1]
                           |> List.filter (\d -> (not (d == (0, 0))))
                           |> List.length))
        ,test "makeBoard 1" (assertEqual 64 (List.length ((makeBoard 8 8) |> .board)))
        ,test "makeBoard 2" (assertEqual {piece=None, pos=(1,1)} (Maybe.withDefault {piece=None, pos=(0,0)} (List.head ((makeBoard 8 8) |> .board))))
        ,test "setPieces 1" (assertEqual 64 ((makeBoard 8 8)
                                          |> setPieces [
                                                {piece = White, pos=(4,4)}
                                               ,{piece = Black, pos=(4,5)}
                                               ,{piece = White, pos=(5,4)}
                                               ,{piece = Black, pos=(5,5)}]
                                          |> .board
                                          |> List.length))
        ,test "setPieces 2" (assertEqual 2 ((makeBoard 8 8)
                                          |> setPieces [
                                                {piece = White, pos=(4,4)}
                                               ,{piece = Black, pos=(4,5)}
                                               ,{piece = White, pos=(5,4)}
                                               ,{piece = Black, pos=(5,5)}]
                                          |> .board
                                          |> List.filter (\s -> White == s.piece)
                                          |> List.length))
        ,test "isOutOfBoard 1" (assertEqual False (isOutOfBoard (makeBoard 8 8) (1,1)))
        ,test "isOutOfBoard 2" (assertEqual True (isOutOfBoard (makeBoard 8 8) (0,0)))
        ,test "isOutOfBoard 3" (assertEqual True (isOutOfBoard (makeBoard 8 8) (9,9)))
        ,test "isOutOfBoard 4" (assertEqual False (isOutOfBoard (makeBoard 8 8) (8,8)))
        ,test "plusPos" (assertEqual (0,0) (plusPos (-2, 10) (2, -10)))
        ,test "getSquare" (assertEqual White (.piece (Maybe.withDefault {piece = None, pos=(99,99)}
                                                          (getSquare (setPieces [{piece = White, pos=(4,4)}] (makeBoard 8 8)) (4,4)))))
        ,test "nextSquare" (assertEqual White (.piece (Maybe.withDefault {piece = None, pos=(99,99)}
                                                          (nextSquare (setPieces [{piece = White, pos=(4,4)}] (makeBoard 8 8)) (-1, 0) (5,4)))))
        ]
