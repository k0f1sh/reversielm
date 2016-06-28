module ReversiDictVer exposing (..)

import List exposing (..)
import Dict exposing (..)

-- 駒
type Piece = Black | White

-- 座標
type alias Position = (Int, Int)

-- 盤
type alias Board =
    { pieces: Dict Position Piece
    , board_size: Position
    }

-- game
type alias Game =
    { board: Board
    , phase: Piece
    }

-- 方向
directions = combinationOf (\d1 -> \d2 -> (d1, d2)) [-1, 0, 1] [-1, 0, 1]
           |> List.filter (\d -> (not (d == (0, 0))))


-- くみあわせ
combinationOf : (a -> b -> c) -> List a -> List b -> List c
combinationOf f a b =
    List.concat (List.map (\n -> List.map (\m -> (f n m)) b) a)

-- オセロを反転
reverse : Piece -> Piece
reverse p =
    case p of
        Black -> White
        White -> Black

-- 盤面生成
emptyBoard : Int -> Int -> Board
emptyBoard w h =
    { pieces = Dict.empty
    , board_size = (w, h)
    }

-- -- 判定関連
isOutOfBoard : Board -> Position -> Bool
isOutOfBoard board pos =
    (fst pos < 1 || fst board.board_size < fst pos) || (snd pos < 1 || snd board.board_size < snd pos)

setPiece : Piece -> Position -> Board -> Board
setPiece piece position board =
    if isOutOfBoard board position then
        board
    else
        {board | pieces = Dict.insert position piece board.pieces}


directionLinePositions : Position -> Position -> Board -> List Position
directionLinePositions direction current board =
    let
        max_length = (max (fst board.board_size) (snd board.board_size))
    in
        [1..max_length]
        |> List.map (\n -> ((n * fst direction), (n * snd direction)))
        |> List.map (\(n, m) -> ((n + fst current), (m + snd current)))
    

-- currnet_posにpieceを置いた場合のdirection方向の返せるマスをかえす
reversiblePositions : Piece -> Position -> Position -> Board -> List Position
reversiblePositions piece direction current board =
    let
        takeReversibles positions result =
            case positions of
                [] -> result
                x::xs ->
                    case Dict.get x board.pieces of
                        Maybe.Nothing -> []
                        Maybe.Just p ->
                            if p == reverse piece then
                                takeReversibles xs (x::result)
                            else
                                result
    in
        case Dict.get current board.pieces of
            Nothing ->
                takeReversibles (directionLinePositions direction current board) []
            Just _ ->
                -- すでに置いてあったら置けない
                []

-- (現在そこに置ける座標, そこに置いて返せるマスのリスト)
candidates : Piece -> Board -> List (Position, List Position)
candidates piece board =
    let
        pos_dirs = combinationOf (\d -> \p -> (d, p))
                   directions -- 方向
                   (combinationOf (\w -> \h -> (w, h)) [1..(fst board.board_size)] [1..(snd board.board_size)]) -- マス
        isCandidate (d, p) =
            let
                rs = (reversiblePositions piece d p board)
            in
                if [] == rs then
                    Nothing
                else
                    Just (p, rs)
    in
        filterMap isCandidate pos_dirs

reversePiece : Position -> Board -> Board
reversePiece position board =
    case (Dict.get position board.pieces) of
        Nothing -> board
        Just piece ->
            {board | pieces = (Dict.insert position (reverse piece) board.pieces)}
                       
putPiece : Position -> Game -> Game
putPiece position game =
    case Dict.get position game.board.pieces of
        Nothing ->
            let
                rs = List.filter (\ (p, s) -> p == position) (candidates game.phase game.board)
                   |> List.map snd
                   |> List.concat
                new_game = 
                    if [] == rs then
                              game
                    else
                        { game | phase = reverse game.phase
                        , board = (List.foldr reversePiece game.board rs)
                        |> setPiece game.phase position
                        }
            in
                if [] == (candidates new_game.phase new_game.board) then
                    -- スキップ
                    {new_game | phase = reverse new_game.phase}
                else
                    new_game
        Just _ ->
            game
                                
-- Game
emptyGame : Int -> Int -> Game
emptyGame w h = { board = ((emptyBoard w h)
                          |> setPiece White (4, 4)
                          |> setPiece White (5, 5)
                          |> setPiece Black (4, 5)
                          |> setPiece Black (5, 4))
                , phase = Black
                }

isGameEnd : Game -> Bool
isGameEnd game =
    [] == (candidates game.phase game.board)
    && [] == (candidates (reverse game.phase) game.board)
