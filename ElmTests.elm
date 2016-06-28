import String
import ReversiDictVer exposing (..)
import ElmTest exposing (..)
tests : Test
tests =
    suite "A Test Suite"
        [
        test "hoge" (assertEqual 8 (4 + 4))
        ,test "emptyBoard" (assertEqual 8 (fst (emptyBoard 8 8).board_size))
        ,test "isOutOfBoard 1" (assertEqual False (isOutOfBoard (emptyBoard 8 8) (4, 2)))
        ,test "isOutOfBoard 2" (assertEqual True (isOutOfBoard (emptyBoard 8 8) (99, 99)))
        ,test "directionLinePositions" (assertEqual (Maybe.Just (5, 4)) ((directionLinePositions (1, 0) (4, 4) (emptyBoard 8 8)) |> List.head))
        ,test "reversiblePositions 1" (assertEqual 3 ((reversiblePositions
                                                           Black
                                                           (1, 0)
                                                           (1, 4)
                                                           ((emptyBoard 8 8)
                                                           |> setPiece White (2, 4)
                                                           |> setPiece White (3, 4)
                                                           |> setPiece White (4, 4)
                                                           |> setPiece Black (5, 4)
                                                           ))
                                                     |> List.length))
        ,test "candidates 1" (assertEqual 1 ((candidates
                                                  Black
                                                  ((emptyBoard 8 8)
                                                  |> setPiece White (2, 4)
                                                  |> setPiece White (3, 4)
                                                  |> setPiece White (4, 4)
                                                  |> setPiece Black (5, 4)
                                                  ))
                                            |> List.length))
        ]


main : Program Never
main =
    runSuite tests
