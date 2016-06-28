import Html exposing (div, button, text, ul, li)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import ReversiDictVer exposing (..)
import Dict

main = 
    beginnerProgram { model = emptyGame 8 8
                    , view = view
                    , update = update}

type Msg = BoardClick Position


view game = 
    div []
        [ div [] [ Html.text ("手番: " ++ (toString game.phase)) ]
        , div [] [ Html.text ("isGameEnd: " ++ (toString (isGameEnd game))) ]
        , div [] [ Html.text ("Black: " ++ (toString ((Dict.values game.board.pieces) |> List.filter (\p -> Black == p) |> List.length))) ]
        , div [] [ Html.text ("White: " ++ (toString ((Dict.values game.board.pieces) |> List.filter (\p -> White == p) |> List.length))) ]
        , svg
            [width "800", height "800"]
            (List.map (\position -> (squareView game.phase position game.board)) (combinationOf (\w -> \h -> (w, h)) [1..(fst game.board.board_size)] [1..(snd game.board.board_size)]))
        ]


squareView : Piece -> Position -> Board -> Svg Msg
squareView piece position board =
    case Dict.get position board.pieces of
        Maybe.Nothing ->
            emptyView piece position board
        Maybe.Just White ->
            pieceView White position "#fcfcfc"
        Maybe.Just Black ->
            pieceView Black position "#3c3c3c"

emptyView : Piece -> Position -> Board -> Svg Msg
emptyView piece position board =
    if List.member position ((candidates piece board) |> List.map (\(p, s) -> p)) then
        candidateView position
    else
        emptyPieceView position

emptyPieceView : Position -> Svg Msg
emptyPieceView position =
    rect [ x (toString (((fst position) - 1) * 80))
         , y (toString (((snd position) - 1) * 80))
         , width "80"
         , height "80"
         , fill "#7fff7f"
         , stroke "#222222"
         , Svg.Events.onMouseDown (BoardClick position)
         ] []

candidateView : Position -> Svg Msg
candidateView position =
    rect [ x (toString (((fst position) - 1) * 80))
         , y (toString (((snd position) - 1) * 80))
         , width "80"
         , height "80"
         , fill "#81F7BE"
         , stroke "#222222"
         , Svg.Events.onMouseDown (BoardClick position)
         ] []
        
pieceView : Piece -> Position -> String -> Svg Msg
pieceView piece position fill_color =
    g [] [ emptyPieceView position
         , circle [ cx (toString (((fst position) * 80) - 40))
                  , cy (toString (((snd position) * 80) - 40))
                  , r "35"
                  , fill fill_color
                  ] []
         ]
        
update (BoardClick position) game = 
    putPiece position game
