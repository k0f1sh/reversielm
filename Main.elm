import Html exposing (div, button, text, ul, li)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Reversi exposing (..)

main = 
    beginnerProgram { model = makeGame 8 8
                    , view = view
                    , update = update}

type Msg = BoardClick Pos


view game = 
    div []
        [ div [] [ Html.text ("手番: " ++ (toString game.phase)) ]
        , div [] [ Html.text ("isGameEnd: " ++ (toString (isGameEnd game))) ]
        , div [] [ Html.text ("Black: " ++ (toString (List.length (List.filter (\s -> s.piece == Black) game.board.squares)))) ]
        , div [] [ Html.text ("White: " ++ (toString (List.length (List.filter (\s -> s.piece == White) game.board.squares)))) ]
        , svg
            [width "800", height "800"]
            (List.map (\s -> squareView s) game.board.squares)
        ]

squareView : Square -> Svg Msg
squareView square =
    case square.piece of
        White -> pieceView square.piece square.pos "#fcfcfc"
        Black -> pieceView square.piece square.pos "#3c3c3c"
        None -> emptyPieceView square.pos

emptyPieceView : Pos -> Svg Msg
emptyPieceView pos =
    rect [ x (toString (((fst pos) - 1) * 80))
         , y (toString (((snd pos) - 1) * 80))
         , width "80"
         , height "80"
         , fill "#7fff7f"
         , stroke "#222222"
         , Svg.Events.onMouseDown (BoardClick pos)
         ] []
        
        
pieceView : Piece -> Pos -> String -> Svg Msg
pieceView piece pos fill_color =
    g [] [ emptyPieceView pos
         , circle [ cx (toString (((fst pos) * 80) - 40))
                 , cy (toString (((snd pos) * 80) - 40))
                 , r "35"
                 , fill fill_color
                 ] []
        ]
        
update (BoardClick pos) game = 
    putPiece pos game
