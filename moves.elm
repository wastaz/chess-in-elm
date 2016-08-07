module Moves exposing (pieceAt, validMoves)

import ChessTypes exposing (..)

pieceAt : List Piece -> BoardPosition -> Maybe Piece
pieceAt pieces pos =
    pieces
    |> List.filter (\(_, _, pcpos) -> pos == pcpos)
    |> List.head

validMoves : Piece -> List BoardPosition
validMoves pc =
    []
