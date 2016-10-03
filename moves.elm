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

        Rook ->
            validRookMoves player pos

        Knight ->
            validKnightMoves player pos

        Bishop ->
            validBishopMoves player pos

        Queen ->
            List.concat
                [ validBishopMoves player pos
                , validRookMoves player pos
                ]

        King ->
            validKingMoves player pos


move : List Piece -> Piece -> BoardPosition -> Maybe (List Piece)
move board piece pos =
    if (validMoves piece |> List.member pos) then
        Just <| performValidMove board piece pos
    else
        Nothing


performValidMove : List Piece -> Piece -> BoardPosition -> List Piece
performValidMove board ( pl, pc, ps ) pos =
    board
        |> List.filter (\( _, _, c ) -> c /= pos)
        |> List.filter ((/=) ( pl, pc, ps ))
        |> (::) ( pl, pc, pos )


filterMaybe : List (Maybe a) -> List a
filterMaybe lst =
    List.filterMap identity lst


incX : Int -> BoardPosition -> BoardPosition
incX i p =
    { p | x = p.x + i }


decX : Int -> BoardPosition -> BoardPosition
decX i p =
    { p | x = p.x - i }


incY : Int -> BoardPosition -> BoardPosition
incY i p =
    { p | y = p.y + i }


decY : Int -> BoardPosition -> BoardPosition
decY i p =
    { p | y = p.y - i }


validPawnMoves : Player -> BoardPosition -> List BoardPosition
validPawnMoves player pos =
    filterMaybe <|
        case player of
            White ->
                [ Just <| incY 1 pos
                , if pos.y == 1 then
                    Just <| incY 2 pos
                  else
                    Nothing
                ]

            Black ->
                [ Just <| decY 1 pos
                , if pos.y == 6 then
                    Just <| decY 2 pos
                  else
                    Nothing
                ]


validKingMoves : Player -> BoardPosition -> List BoardPosition
validKingMoves player pos =
    [ -1, 0, 1 ]
        |> List.map (\x -> [ -1, 0, 1 ] |> List.map (\y -> (incX x >> incY y) pos))
        |> List.concat
        |> List.filter ((/=) pos)

validRookMoves : Player -> BoardPosition -> List BoardPosition
validRookMoves player pos =
    List.concat
        [ [0..7] |> List.map (\x -> { pos | x = x })
        , [0..7] |> List.map (\y -> { pos | y = y })
        ]
        |> List.filter ((/=) pos)


validBishopMoves : Player -> BoardPosition -> List BoardPosition
validBishopMoves player pos =
    List.concat
        [ [1..pos.x]
            |> List.map
                (\i ->
                    [ (decX i >> incY i) pos
                    , (decX i >> decY i) pos
                    ]
                )
            |> List.concat
        , [1..(7 - pos.x)]
            |> List.map
                (\i ->
                    [ (incX i >> incY i) pos
                    , (incX i >> decY i) pos
                    ]
                )
            |> List.concat
        ]


validKnightMoves : Player -> BoardPosition -> List BoardPosition
validKnightMoves player pos =
    [ ( 2, 1 ), ( -2, 1 ), ( 2, -1 ), ( -2, -1 ), ( 1, 2 ), ( -1, 2 ), ( 1, -2 ), ( -1, -2 ) ]
        |> List.map (\( x, y ) -> (incX x >> incY y) pos)
        |> List.filter (\p -> p.x >= 0 && p.y >= 0)
