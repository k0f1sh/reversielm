module Tests where

import ElmTest exposing (..)

import String
import Reversi exposing (..)

all : Test
all =
    suite "A Test Suite"
        [
         test "Addition" (assertEqual (3 + 7) 10)
        ,test "Reverse Head" (assertEqual Head (reverse Tail))
        ,test "Reverse Tail" (assertEqual Tail (reverse Head))
        ,test "Put Piece 1" (assertEqual (Ok (Just Head)) (put Nothing Head))
        ,test "Put Piece 1" (assertEqual (Err "not empty square...") (put (Just Tail) Head))
        ]
