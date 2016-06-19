module Reversi where

import List exposing (..)

-- 駒
type Piece = Black | White | None

-- 座標
type alias Pos = (Int, Int) 

-- 1マス
type alias Square = 
    { piece: Piece
    , pos: Pos
    }

-- WxH
type alias BoardSize = (Int, Int)

-- 盤
type alias Board =
    { board: List Square
    , board_size: BoardSize
    }

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
        {board = map2 makeSquare (repeat (length poss) None) poss
        ,board_size = (w, h)}

setPieces : List Square -> Board -> Board
setPieces l b =
    let
        board = map (\s -> (filter (\t -> s.pos == t.pos) l) |> head |> Maybe.withDefault s) b.board
    in
        {board = board
        ,board_size = b.board_size}

-- -- 判定関連
isOutOfBoard : Board -> Pos -> Bool
isOutOfBoard b pos = 
    let
        out_w = fst b.board_size + 1
        out_h = snd b.board_size + 1
    in
    case pos of
        (0, _) -> True
        (_, 0) -> True
        (w, h) ->
            if (w >= out_w) then
                True
            else if (h >= out_h) then
                True
            else
                False

plusPos : Pos -> Pos -> Pos
plusPos pos1 pos2 =
    ((fst pos1 + fst pos2), (snd pos1 + snd pos2))

getSquareFromList : List Square -> Pos -> Maybe Square
getSquareFromList b p =
    case b of
        [] -> Maybe.Nothing
        x::xs -> if x.pos == p then
                     Maybe.Just x
                 else
                     getSquareFromList xs p

getSquare : Board -> Pos -> Maybe Square
getSquare b p =
    getSquareFromList b.board p
        

nextSquare : Board -> Pos -> Pos -> Maybe Square
nextSquare b direction current_pos =
    let
        new_pos = (plusPos direction current_pos)
    in
        if (isOutOfBoard b new_pos) then
            Maybe.Nothing
        else
            getSquare b new_pos

-- オセロを反転
reverse : Piece -> Piece
reverse p = 
    case p of
        Black -> White
        White -> Black
        None -> None

