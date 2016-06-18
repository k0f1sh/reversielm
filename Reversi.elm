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
    List.concat (List.map (\n -> List.map (\m -> (f n m)) b) a)

-- 盤面生成
makePos : Int -> Int -> Pos
makePos n m =
    (n, m)
    
makeBoard : Int -> Int -> Board
makeBoard w h =
    let
        squares = 
            combinationOf makePos [1..w] [1..h]
    in
        combinationOf (\s -> \p -> {piece=p, pos=s}) squares (List.repeat (List.length squares) None)
      
-- オセロを反転
reverse : Piece -> Piece
reverse p = 
    case p of
        Black -> White
        White -> Black
        None -> None

