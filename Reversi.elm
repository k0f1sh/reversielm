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
    { squares: List Square
    , board_size: BoardSize
    }

-- 盤面
w = 8
h = 8
board = combinationOf (\a -> \b -> {piece = a, pos = b}) [1..w] [1..h]
directions = combinationOf (\d1 -> \d2 -> (d1, d2)) [-1, 0, 1] [-1, 0, 1]
             |> filter (\d -> (not (d == (0, 0))))

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
        {squares = map2 makeSquare (repeat (length poss) None) poss
        ,board_size = (w, h)}

setPiece : Square -> Board -> Board
setPiece s b =
    let
        squares = map (\t ->
                           if t.pos == s.pos then
                               s
                           else
                               t
                      ) b.squares
    in
        {b | squares = squares}

-- -- 判定関連
isOutOfBoard : Board -> Pos -> Bool
isOutOfBoard board pos =
  let bound f = f pos < 1 || f board.board_size < f pos
  in bound fst || bound snd

plusPos : Pos -> Pos -> Pos
plusPos pos1 pos2 =
    ((fst pos1 + fst pos2), (snd pos1 + snd pos2))

getSquareFromList : List Square -> Pos -> Maybe Square
getSquareFromList s p =
    case s of
        [] -> Maybe.Nothing
        x::xs -> if x.pos == p then
                     Maybe.Just x
                 else
                     getSquareFromList xs p

getSquare : Board -> Pos -> Maybe Square
getSquare b p =
    getSquareFromList b.squares p
        
multiplyPos : Pos -> Int -> Pos
multiplyPos p n =
    (((fst p) * n), ((snd p) * n))

-- currnet_posにpieceを置いた場合のdirection方向の返せるマスをかえす
reversibleSquares : Piece -> Pos -> Pos -> Board -> List Square
reversibleSquares piece direction current_pos board =
    let
        max_length = (max (fst board.board_size) (snd board.board_size))
        direction_squares = [1..max_length]
                          |> map (\n -> multiplyPos direction n)
                          |> map (\p -> plusPos current_pos p)
                          |> filterMap (\p -> getSquare board p)
        outOfBoardSquares = take 1 (List.reverse direction_squares) |> map (\s -> {s | pos = (plusPos s.pos direction), piece = None})
        takeReversibles l result =
            case l of
                [] -> result
                x::xs ->
                    if x.piece == None then
                        []
                    else
                        if x.piece == reverse piece then
                            takeReversibles xs (x::result)
                        else
                            result
    in
        takeReversibles (append direction_squares outOfBoardSquares) []

-- オセロを反転
reverse : Piece -> Piece
reverse p = 
    case p of
        Black -> White
        White -> Black
        None -> None
