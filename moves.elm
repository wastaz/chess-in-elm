module Moves exposing (pieceAt, validMoves, move)

import Maybe
import ChessTypes exposing (..)
import Util exposing (..)


pieceAt : List Piece -> BoardPosition -> Maybe Piece
pieceAt pieces pos =
    pieces
        |> List.filter (.position >> (==) pos)
        |> List.head


validMoves : Board -> Piece -> List BoardPosition
validMoves board piece =
    generateMoves board piece
        |> List.filter (not << putsKingInCheck board piece)


generateMoves : Board -> Piece -> List BoardPosition
generateMoves board piece =
    case piece.kind of
        Pawn ->
            validPawnMoves board piece.owner piece.position

        Rook ->
            validRookMoves board piece.owner piece.position

        Knight ->
            validKnightMoves board piece.owner piece.position

        Bishop ->
            validBishopMoves board piece.owner piece.position

        Queen ->
            List.concat
                [ validBishopMoves board piece.owner piece.position
                , validRookMoves board piece.owner piece.position
                ]

        King ->
            validKingMoves board piece


move : List Piece -> Piece -> BoardPosition -> Maybe (List Piece)
move board piece pos =
    if (validMoves board piece |> List.member pos) then
        Just <| performValidMove board piece pos
    else
        Nothing


performValidMove : List Piece -> Piece -> BoardPosition -> List Piece
performValidMove board piece pos =
    board
        |> List.filter (.position >> (/=) pos)
        |> List.filter ((/=) piece)
        |> (::) { piece | position = pos }


kingsThatCannotMove : List Piece -> List Piece
kingsThatCannotMove board =
    board
        |> List.filter (\p -> p.kind == King && (not <| List.isEmpty <| validMoves board p))


piecesThreatening : List Piece -> Piece -> List Piece
piecesThreatening board piece =
    board
        |> List.filter (.owner >> (/=) piece.owner)
        |> List.filter (validMoves board >> List.member piece.position)


findKing : Player -> Board -> Piece
findKing player board =
    board
        |> List.filter (\p -> p.owner == player && p.kind == King)
        |> List.head
        |> Maybe.withDefault { owner = player, kind = King, position = { x = 0, y = 0 } }


putsKingInCheck : Board -> Piece -> BoardPosition -> Bool
putsKingInCheck board piece pos =
    let
        newboard =
            performValidMove board piece pos

        myking =
            newboard |> findKing piece.owner
    in
        newboard
            |> List.filter (.owner >> (/=) piece.owner)
            |> List.filter (\p -> p |> generateMoves newboard |> List.member myking.position)
            |> (not << List.isEmpty)


hasAnyPiece : List Piece -> BoardPosition -> Bool
hasAnyPiece board =
    pieceAt board >> isJust


hasPlayerPiece : Player -> List Piece -> BoardPosition -> Bool
hasPlayerPiece player board =
    pieceAt board >> Maybe.map (.owner >> (==) player) >> Maybe.withDefault False


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


getPlayerFirstRowY : Player -> Int
getPlayerFirstRowY player =
    case player of
        White ->
            1

        Black ->
            6


validPawnMoves : List Piece -> Player -> BoardPosition -> List BoardPosition
validPawnMoves board player pos =
    let
        moveY =
            case player of
                White ->
                    incY

                Black ->
                    decY

        firstRowIdx =
            getPlayerFirstRowY player

        singleMove =
            moveY 1 pos

        doubleMove =
            moveY 2 pos

        leftKillMove =
            pos |> moveY 1 |> decX 1

        rightKillMove =
            pos |> moveY 1 |> incX 1
    in
        [ maybeFromPredicate (not << hasAnyPiece board) singleMove
        , maybeFromPredicate (\m -> pos.y == firstRowIdx && not (hasAnyPiece board m)) doubleMove
        , maybeFromPredicate (hasPlayerPiece (other player) board) leftKillMove
        , maybeFromPredicate (hasPlayerPiece (other player) board) rightKillMove
        ]
            |> filterMaybe


validKingMoves : List Piece -> Piece -> List BoardPosition
validKingMoves board piece =
    [ -1, 0, 1 ]
        |> List.map (\x -> [ -1, 0, 1 ] |> List.map (\y -> (incX x >> incY y) piece.position))
        |> List.concat
        |> List.filter ((/=) piece.position)
        |> List.filter (not << hasPlayerPiece piece.owner board)


validRookMoves : List Piece -> Player -> BoardPosition -> List BoardPosition
validRookMoves board player pos =
    List.concat
        [ [pos.x + 1..7] |> List.map (\x -> { pos | x = x }) |> takeUntil (hasAnyPiece board)
        , [0..pos.x - 1] |> List.reverse |> List.map (\x -> { pos | x = x }) |> takeUntil (hasAnyPiece board)
        , [pos.y + 1..7] |> List.map (\y -> { pos | y = y }) |> takeUntil (hasAnyPiece board)
        , [0..pos.y - 1] |> List.reverse |> List.map (\y -> { pos | y = y }) |> takeUntil (hasAnyPiece board)
        ]
        |> List.filter (not << hasPlayerPiece player board)


validBishopMoves : List Piece -> Player -> BoardPosition -> List BoardPosition
validBishopMoves board player pos =
    List.concat
        [ [pos.x + 1..7] |> List.indexedMap (\i x -> { pos | x = x, y = pos.y + i + 1 }) |> takeUntil (hasAnyPiece board)
        , [pos.x + 1..7] |> List.indexedMap (\i x -> { pos | x = x, y = pos.y - (i + 1) }) |> takeUntil (hasAnyPiece board)
        , [0..pos.x - 1] |> List.reverse |> List.indexedMap (\i x -> { pos | x = x, y = pos.y + i + 1 }) |> takeUntil (hasAnyPiece board)
        , [0..pos.x - 1] |> List.reverse |> List.indexedMap (\i x -> { pos | x = x, y = pos.y - (i + 1) }) |> takeUntil (hasAnyPiece board)
        ]
        |> List.filter (\p -> p.y >= 0 && p.y < 8)
        |> List.filter (not << hasPlayerPiece player board)


validKnightMoves : List Piece -> Player -> BoardPosition -> List BoardPosition
validKnightMoves board player pos =
    [ ( 2, 1 ), ( -2, 1 ), ( 2, -1 ), ( -2, -1 ), ( 1, 2 ), ( -1, 2 ), ( 1, -2 ), ( -1, -2 ) ]
        |> List.map (\( x, y ) -> (incX x >> incY y) pos)
        |> List.filter (\p -> p.x >= 0 && p.y >= 0)
        |> List.filter (not << hasPlayerPiece player board)
