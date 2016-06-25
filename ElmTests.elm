import String
import Reversi exposing (..)
import ElmTest exposing (..)

tests : Test
tests =
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
        ,test "makeBoard 1" (assertEqual 64 (List.length ((makeBoard 8 8) |> .squares)))
        ,test "makeBoard 2" (assertEqual {piece=None, pos=(1,1)} (Maybe.withDefault {piece=None, pos=(0,0)} (List.head ((makeBoard 8 8) |> .squares))))
        ,test "setPiece 1" (assertEqual 64 ((makeBoard 8 8)
                                           |> setPiece {piece = White, pos=(4,4)}
                                           |> setPiece {piece = White, pos=(5,5)}
                                           |> setPiece {piece = Black, pos=(4,5)}
                                           |> setPiece {piece = Black, pos=(5,4)}
                                           |> .squares
                                           |> List.length))
        ,test "setPiece 2" (assertEqual 2 ((makeBoard 8 8)
                                          |> setPiece {piece = White, pos=(4,4)}
                                          |> setPiece {piece = White, pos=(5,5)}
                                          |> setPiece {piece = Black, pos=(4,5)}
                                          |> setPiece {piece = Black, pos=(5,4)}
                                          |> .squares
                                          |> List.filter (\s -> White == s.piece)
                                          |> List.length))
        ,test "isOutOfBoard 1" (assertEqual False (isOutOfBoard (makeBoard 8 8) (1,1)))
        ,test "isOutOfBoard 2" (assertEqual True (isOutOfBoard (makeBoard 8 8) (0,0)))
        ,test "isOutOfBoard 3" (assertEqual True (isOutOfBoard (makeBoard 8 8) (9,9)))
        ,test "isOutOfBoard 4" (assertEqual False (isOutOfBoard (makeBoard 8 8) (8,8)))
        ,test "plusPos" (assertEqual (0,0) (plusPos (-2, 10) (2, -10)))
        ,test "getSquare" (assertEqual White (.piece (Maybe.withDefault {piece = None, pos=(99,99)}
                                                          (getSquare (setPiece {piece = White, pos=(4,4)} (makeBoard 8 8)) (4,4)))))
        ,test "reversibleSquares 1" (assertEqual 3 ((makeBoard 8 8)
                                                   |> setPiece {piece = White, pos=(2,4)}
                                                   |> setPiece {piece = White, pos=(3,4)}
                                                   |> setPiece {piece = White, pos=(4,4)}
                                                   |> setPiece {piece = Black, pos=(5,4)}
                                                   |> reversibleSquares Black (1, 0) (1, 4)
                                                   |> List.length))
        ,test "reversibleSquares 2" (assertEqual 0 ((makeBoard 8 8)
                                                   |> setPiece {piece = White, pos=(2,4)}
                                                   |> setPiece {piece = White, pos=(3,4)}
                                                   |> setPiece {piece = White, pos=(4,4)}
                                                   |> reversibleSquares White (1, 0) (1, 4)
                                                   |> List.length))
        ,test "reversibleSquares 3" (assertEqual 0 ((makeBoard 8 8)
                                                   |> reversibleSquares None (1, 0) (1, 4)
                                                   |> List.length))
        ,test "reversibleSquares 4" (assertEqual 0 ((makeBoard 8 8)
                                                   |> setPiece {piece = White, pos=(5,4)}
                                                   |> setPiece {piece = White, pos=(6,4)}
                                                   |> setPiece {piece = White, pos=(7,4)}
                                                   |> setPiece {piece = White, pos=(8,4)}
                                                   |> reversibleSquares Black (1, 0) (4, 4)
                                                   |> List.length))
        ,test "candidate 1" (assertEqual 4 ((makeBoard 8 8)
                                           |> setPiece {piece = White, pos=(4,4)}
                                           |> setPiece {piece = White, pos=(5,5)}
                                           |> setPiece {piece = Black, pos=(4,5)}
                                           |> setPiece {piece = Black, pos=(5,4)}
                                           |> candidates Black
                                           |> List.length))
        ,test "candidate 2" (assertEqual (Maybe.Just 3) ((makeBoard 8 8)
                                                        |> setPiece {piece = Black, pos=(3,4)}
                                                        |> setPiece {piece = Black, pos=(4,4)}
                                                        |> setPiece {piece = White, pos=(5,5)}
                                                        |> setPiece {piece = Black, pos=(4,5)}
                                                        |> setPiece {piece = Black, pos=(5,4)}
                                                        |> setPiece {piece = White, pos=(6,4)}
                                                        |> candidates White
                                                        |> List.map (\t -> snd t |> List.length)
                                                        |> List.maximum))
        ,test "isGameEnd 1" (assertEqual True (isGameEnd {board = ((makeBoard 8 8)
                                                                |> setPiece {piece = Black, pos=(3,4)}
                                                                |> setPiece {piece = Black, pos=(4,4)}
                                                                |> setPiece {piece = Black, pos=(4,5)}
                                                                |> setPiece {piece = Black, pos=(5,4)})
                                                       , phase = White}))
        ,test "isGameEnd 2" (assertEqual False (isGameEnd {board = ((makeBoard 8 8)
                                                                |> setPiece {piece = White, pos=(3,4)}
                                                                |> setPiece {piece = Black, pos=(4,4)}
                                                                |> setPiece {piece = Black, pos=(4,5)}
                                                                |> setPiece {piece = Black, pos=(5,4)})
                                                       , phase = Black}))
        ]


main : Program Never
main =
    runSuite tests
