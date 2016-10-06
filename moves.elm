module Moves exposing (pieceAt, validMoves, move)

import Maybe
import ChessTypes exposing (..)
import Util exposing (..)


pieceAt : List Piece -> BoardPosition -> Maybe Piece
pieceAt pieces pos =
    pieces
        |> List.filter (.position >> (==) pos)
        |> List.head


validMoves : Board -> Piece -> List Move
validMoves board piece =
    generateMoves board piece
        |> List.filter (not << putsKingInCheck board)


generateMoves : Board -> Piece -> List Move
generateMoves board piece =
    case piece.kind of
        Pawn ->
            validPawnMoves board piece

        Rook ->
            validRookMoves board piece

        Knight ->
            validKnightMoves board piece

        Bishop ->
            validBishopMoves board piece

        Queen ->
            List.concat
                [ validBishopMoves board piece
                , validRookMoves board piece
                ]

        King ->
            validKingMoves board piece


move : Board -> Piece -> BoardPosition -> Maybe Board
move board piece pos =
    if (validMoves board piece |> List.map .position |> List.member pos) then
        Just <| performValidMove board piece pos
    else
        Nothing


performValidMove : Board -> Piece -> BoardPosition -> List Piece
performValidMove board piece pos =
    board
        |> List.filter (.position >> (/=) pos)
        |> List.filter ((/=) piece)
        |> (::) { piece | position = pos }


kingsThatCannotMove : List Piece -> List Piece
kingsThatCannotMove board =
    board
        |> List.filter (\p -> p.kind == King && (not <| List.isEmpty <| validMoves board p))


findKing : Player -> Board -> Piece
findKing player board =
    board
        |> List.filter (\p -> p.owner == player && p.kind == King)
        |> List.head
        |> Maybe.withDefault { owner = player, kind = King, position = { x = 0, y = 0 } }


putsKingInCheck : Board -> Move -> Bool
putsKingInCheck board { piece, position } =
    let
        newboard =
            performValidMove board piece position

        myking =
            newboard |> findKing piece.owner
    in
        newboard
            |> List.filter (.owner >> (/=) piece.owner)
            |> List.filter (\p -> p |> generateMoves newboard |> List.map .position |> List.member myking.position)
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


validPawnMoves : List Piece -> Piece -> List Move
validPawnMoves board piece =
    let
        moveY =
            case piece.owner of
                White ->
                    incY

                Black ->
                    decY

        firstRowIdx =
            getPlayerFirstRowY piece.owner

        singleMove =
            moveY 1 piece.position

        doubleMove =
            moveY 2 piece.position

        leftKillMove =
            piece.position |> moveY 1 |> decX 1

        rightKillMove =
            piece.position |> moveY 1 |> incX 1
    in
        [ maybeFromPredicate (not << hasAnyPiece board) singleMove
        , maybeFromPredicate (\m -> piece.position.y == firstRowIdx && not (hasAnyPiece board m)) doubleMove
        , maybeFromPredicate (hasPlayerPiece (other piece.owner) board) leftKillMove
        , maybeFromPredicate (hasPlayerPiece (other piece.owner) board) rightKillMove
        ]
            |> filterMaybe
            |> toMoves board piece


validKingMoves : Board -> Piece -> List Move
validKingMoves board piece =
    [ -1, 0, 1 ]
        |> List.map (\x -> [ -1, 0, 1 ] |> List.map (\y -> (incX x >> incY y) piece.position))
        |> List.concat
        |> List.filter ((/=) piece.position)
        |> List.filter (not << hasPlayerPiece piece.owner board)
        |> toMoves board piece


validRookMoves : Board -> Piece -> List Move
validRookMoves board piece =
    let
        pos =
            piece.position
    in
        List.concat
            [ [pos.x + 1..7] |> List.map (\x -> { pos | x = x }) |> takeUntil (hasAnyPiece board)
            , [0..pos.x - 1] |> List.reverse |> List.map (\x -> { pos | x = x }) |> takeUntil (hasAnyPiece board)
            , [pos.y + 1..7] |> List.map (\y -> { pos | y = y }) |> takeUntil (hasAnyPiece board)
            , [0..pos.y - 1] |> List.reverse |> List.map (\y -> { pos | y = y }) |> takeUntil (hasAnyPiece board)
            ]
            |> List.filter (not << hasPlayerPiece piece.owner board)
            |> toMoves board piece


validBishopMoves : List Piece -> Piece -> List Move
validBishopMoves board piece =
    let
        pos =
            piece.position
    in
        List.concat
            [ [pos.x + 1..7] |> List.indexedMap (\i x -> { pos | x = x, y = pos.y + i + 1 }) |> takeUntil (hasAnyPiece board)
            , [pos.x + 1..7] |> List.indexedMap (\i x -> { pos | x = x, y = pos.y - (i + 1) }) |> takeUntil (hasAnyPiece board)
            , [0..pos.x - 1] |> List.reverse |> List.indexedMap (\i x -> { pos | x = x, y = pos.y + i + 1 }) |> takeUntil (hasAnyPiece board)
            , [0..pos.x - 1] |> List.reverse |> List.indexedMap (\i x -> { pos | x = x, y = pos.y - (i + 1) }) |> takeUntil (hasAnyPiece board)
            ]
            |> List.filter (\p -> p.y >= 0 && p.y < 8)
            |> List.filter (not << hasPlayerPiece piece.owner board)
            |> toMoves board piece


validKnightMoves : List Piece -> Piece -> List Move
validKnightMoves board piece =
    [ ( 2, 1 ), ( -2, 1 ), ( 2, -1 ), ( -2, -1 ), ( 1, 2 ), ( -1, 2 ), ( 1, -2 ), ( -1, -2 ) ]
        |> List.map (\( x, y ) -> (incX x >> incY y) piece.position)
        |> List.filter (\p -> p.x >= 0 && p.y >= 0)
        |> List.filter (not << hasPlayerPiece piece.owner board)
        |> toMoves board piece


toMoves : Board -> Piece -> List BoardPosition -> List Move
toMoves board piece moves =
    moves
        |> List.map
            (\m ->
                { piece = piece
                , position = m
                , moveType =
                    if hasPlayerPiece (other piece.owner) board m then
                        Attack
                    else
                        Normal
                }
            )
