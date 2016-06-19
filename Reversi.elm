module Reversi where

import List exposing (..)

-- 駒
type Piece = Black | White | None

type alias Pos = (Int, Int) -- 座標
-- 1マス
type alias Square = 
    { piece: Piece
    , pos: Pos
    }
type alias Board = List Square -- 盤

-- 盤面
w = 8
h = 8
board = combinationOf (\a -> \b -> {piece = a, pos = b}) [1..w] [1..h]

-- くみあわせ
combinationOf : (a -> b -> c) -> List a -> List b -> List c
combinationOf f a b =
    concat (map (\n -> map (\m -> (f n m)) b) a)

-- 盤面生成
makePos : Int -> Int -> Pos
makePos n m =
    (n, m)

makeSquare: Piece -> Pos -> Square
makeSquare pi po =
    { piece = pi
    , pos = po
    }
    
makeBoard : Int -> Int -> Board
makeBoard w h =
    let
        poss = 
            combinationOf makePos [1..w] [1..h]
    in
        map2 makeSquare (repeat (length poss) None) poss

setPieces : List Square -> Board -> Board
setPieces l b =
    map (\s ->
             let
                 filtered = (filter (\t -> s.pos == t.pos) l)
             in
                 (Maybe.withDefault s (head filtered))
        )
        b
      
-- オセロを反転
reverse : Piece -> Piece
reverse p = 
    case p of
        Black -> White
        White -> Black
        None -> None

