module ChessTypes exposing (..)

import Mouse exposing (Position)
import String

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


type alias Piece =
    { owner : Player
    , kind : ChessPiece
    , position : BoardPosition
    }


type alias Board =
    List Piece


type alias Model =
    { board : Board
    , markedSquares : List BoardPosition
    , activePiece : Maybe Piece
    , activePlayer : Player
    }


type Msg
    = Noop
    | ClickAt Position


other : Player -> Player
other player =
    case player of
        White ->
            Black

        Black ->
            White


initialBoard : List Piece
initialBoard =
    List.concat
        [ List.map (\i -> { owner = White, kind = Pawn, position = { x = i, y = 1 } }) [0..7]
        , List.map (\i -> { owner = Black, kind = Pawn, position = { x = i, y = 6 } }) [0..7]
        , [ ( White, 0 ), ( Black, 7 ) ]
            |> List.map
                (\( clr, y ) ->
                    [ Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook ]
                        |> List.indexedMap (\x pc -> { owner = clr, kind = pc, position = { x = x, y = y } })
                )
            |> List.concat
        ]


pieceToStr : Piece -> String
pieceToStr piece =
    case piece.kind of
        Pawn -> "P"
        Rook -> "R"
        Knight -> "K"
        Bishop -> "B"
        Queen -> "Q"
        King -> "K"

boardToStr : Board -> String
boardToStr board =
    [0..7]
    |> List.map (\y -> 
        [0..7] 
        |> List.map (\x ->
            board
            |> List.filter (.position >> (==) { x = x, y = y})
            |> List.map pieceToStr
            |> List.head
            |> Maybe.withDefault "_"
        )
        |> String.concat
        |> \s -> s ++ (String.fromChar '\n')
    )
    |> String.concat