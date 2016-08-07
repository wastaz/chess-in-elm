module ChessTypes exposing (..)

type ChessPiece
    = Pawn
    | Rook
    | Knight
    | Bishop
    | Queen
    | King

type Player
    = Black
    | White

type alias Position = (Int, Int)

type alias Piece = (Player, ChessPiece, Position)

other player =
    case player of
        White -> Black
        Black -> White
