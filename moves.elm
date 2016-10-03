module Moves exposing (pieceAt, validMoves, move)

import Maybe
import ChessTypes exposing (..)


pieceAt : List Piece -> BoardPosition -> Maybe Piece
pieceAt pieces pos =
    pieces
        |> List.filter (\( _, _, pcpos ) -> pos == pcpos)
        |> List.head


validMoves : Piece -> List BoardPosition
validMoves ( player, piece, pos ) =
    case piece of
        Pawn ->
            validPawnMoves player pos

        _ ->
            []


move : List Piece -> Piece -> BoardPosition -> Maybe (List Piece)
move board piece pos =
    if (validMoves piece |> List.member pos) then
        Just <| performValidMove board piece pos
    else
        Nothing


performValidMove : List Piece -> Piece -> BoardPosition -> List Piece
performValidMove board ( pl, pc, ps ) pos =
    board
        |> List.filter ((/=) ( pl, pc, ps ))
        |> (::) ( pl, pc, pos )


filterMaybe : List (Maybe a) -> List a
filterMaybe lst =
    List.filterMap identity lst


validPawnMoves : Player -> BoardPosition -> List BoardPosition
validPawnMoves player pos =
    filterMaybe <|
        case player of
            White ->
                [ Just { pos | y = pos.y + 1 }
                , if pos.y == 1 then
                    Just { pos | y = pos.y + 2 }
                  else
                    Nothing
                ]

            Black ->
                [ Just { pos | y = pos.y - 1 }
                , if pos.y == 6 then
                    Just { pos | y = pos.y - 2 }
                  else
                    Nothing
                ]
