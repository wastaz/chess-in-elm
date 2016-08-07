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

type alias BoardPosition =
    { x : Int
    , y : Int
    }

type alias Piece = (Player, ChessPiece, BoardPosition)

type alias Model =
    { board : List Piece
    , markedSquares : List BoardPosition
    , activePlayer : Player
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
            List.map (\i -> (White, Pawn, { x = i, y = 1})) [0 .. 7],
            List.map (\i -> (Black, Pawn, { x = i, y = 6})) [0 .. 7],
            [ (White, 0), (Black, 7)]
            |> List.map (\(clr, y) ->
                    [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
                    |> List.indexedMap (\x pc -> (clr, pc, { x = x, y = y}))
                )
            |> List.concat
        ]
