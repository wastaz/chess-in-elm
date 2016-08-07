module ChessTypes exposing (..)

import Mouse exposing (Position)

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

type alias Coord = (Int, Int)

type alias Piece = (Player, ChessPiece, Coord)

type alias Model =
    { board : List Piece
    , markedSquare : Maybe Coord
    }

type Msg
    = Noop
    | ClickAt Position

other : Player -> Player
other player =
    case player of
        White -> Black
        Black -> White

initialBoard : List Piece
initialBoard =
    List.concat
        [
            List.map (\i -> (White, Pawn, (i, 1))) [0 .. 7],
            List.map (\i -> (Black, Pawn, (i, 6))) [0 .. 7],
            [ (White, 0), (Black, 7)]
            |> List.map (\(clr, y) ->
                    [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
                    |> List.indexedMap (\x pc -> (clr, pc, (x, y)))
                )
            |> List.concat
        ]
