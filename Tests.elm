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
        ,test "makeBoard" (assertEqual {piece=None, pos=(1,1)} (Maybe.withDefault {piece=None, pos=(0,0)}(List.head (makeBoard 8 8))))
        ]
