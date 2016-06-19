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
        ,test "makeBoard 1" (assertEqual 64 (List.length (makeBoard 8 8)))
        ,test "makeBoard 2" (assertEqual {piece=None, pos=(1,1)} (Maybe.withDefault {piece=None, pos=(0,0)}(List.head (makeBoard 8 8))))
        ,test "setPieces 1" (assertEqual 64 ((makeBoard 8 8)
                                          |> setPieces [
                                                {piece = White, pos=(4,4)}
                                               ,{piece = Black, pos=(4,5)}
                                               ,{piece = White, pos=(5,4)}
                                               ,{piece = Black, pos=(5,5)}]
                                          |> List.length))
        ,test "setPieces 2" (assertEqual 2 ((makeBoard 8 8)
                                          |> setPieces [
                                                {piece = White, pos=(4,4)}
                                               ,{piece = Black, pos=(4,5)}
                                               ,{piece = White, pos=(5,4)}
                                               ,{piece = Black, pos=(5,5)}]
                                          |> List.filter (\s -> White == s.piece)
                                          |> List.length))
        ]
