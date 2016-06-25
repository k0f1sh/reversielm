import Html exposing (div, button, text, ul, li)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Reversi exposing (..)

main = 
    beginnerProgram { model = makeGame 8 8
                    , view = view
                    , update = update}


view game = 
    div []
        [ div [] [ Html.text ("手番: " ++ (toString game.phase))
                 ]
        , svg
            [width "800", height "800"]
            (List.map (\s -> squareView s) game.board.squares)
        ]

squareView : Square -> Svg msg
squareView square =
    case square.piece of
        White -> pieceView square.piece square.pos "#fcfcfc"
        Black -> pieceView square.piece square.pos "#3c3c3c"
        None -> emptyPieceView square.pos

emptyPieceView : Pos -> Svg msg
emptyPieceView pos =
    rect [ x (toString (((fst pos) - 1) * 80))
         , y (toString (((snd pos) - 1) * 80))
         , width "80"
         , height "80"
         , fill "#7fff7f"
         , stroke "#222222"
         ] []
        
        
pieceView : Piece -> Pos -> String -> Svg msg
pieceView piece pos fill_color =
    g [] [ emptyPieceView pos
         , circle [ cx (toString (((fst pos) * 80) - 40))
                 , cy (toString (((snd pos) * 80) - 40))
                 , r "35"
                 , fill fill_color
                 ] []
        ]
        
update msg model = 
    model
