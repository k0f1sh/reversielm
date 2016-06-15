module Reversi where

-- 駒
type Piece = Head | Tail

-- Maybe Piece を Squareとして扱いたい

-- オセロを反転
reverse : Piece -> Piece
reverse p = 
    case p of
        Head -> Tail
        Tail -> Head

-- マスに駒を置く
put : Maybe Piece -> Piece -> Result String (Maybe Piece) 
put m p =
    case m of
        Just _ -> Err "not empty square..."
        Nothing -> Ok (Just p)
