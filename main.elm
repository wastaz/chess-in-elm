module Main exposing (..)

import Html.App as App
import Mouse exposing (Position)
import ChessTypes exposing (..)
import View exposing (view)
import Moves exposing (pieceAt, validMoves, move)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { board = initialBoard
      , possibleMoves = []
      , activePlayer = White
      , activePiece = Nothing
      }
    , Cmd.none
    )


positionToCoord : Position -> Maybe BoardPosition
positionToCoord pos =
    let
        x =
            (pos.x - 5) // 100

        y =
            (pos.y - 5) // 100
    in
        if x > 7 || y > 7 then
            Nothing
        else
            Just { x = x, y = 7 - y }


performMove : Model -> BoardPosition -> Piece -> Model
performMove model pos pc =
    case move model.board pc pos of
        Nothing ->
            let
                existing =
                    pieceAt model.board pos

                pl =
                    Maybe.map (.owner) existing |> Maybe.withDefault model.activePlayer
            in
                if pl == model.activePlayer then
                    { model | activePiece = existing }
                else
                    model

        Just b ->
            { model
                | board = b
                , activePiece = Nothing
                , activePlayer = other model.activePlayer
            }


translateClickOnEmptySquare : Model -> BoardPosition -> Model
translateClickOnEmptySquare model pos =
    model.activePiece
        |> Maybe.map (performMove model pos)
        |> Maybe.withDefault { model | activePiece = Nothing }


translateClickOnPieceSquare : Model -> BoardPosition -> Model
translateClickOnPieceSquare model pos =
    let
        clickedPiece =
            pieceAt model.board pos
    in
        case ( clickedPiece, model.activePiece ) of
            ( Nothing, _ ) ->
                translateClickOnEmptySquare model pos

            ( Just p, Nothing ) ->
                if p.owner == model.activePlayer then
                    { model | activePiece = Just p }
                else
                    model

            ( Just p, Just ap ) ->
                translateClickOnEmptySquare model pos


translateClick : Model -> Position -> Model
translateClick model pos =
    pos
        |> positionToCoord
        |> Maybe.map (translateClickOnPieceSquare model)
        |> Maybe.withDefault { model | activePiece = Nothing }


withValidMoves : Model -> Model
withValidMoves model =
    { model | possibleMoves = model.activePiece |> Maybe.map (validMoves model.board) |> Maybe.withDefault [] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ClickAt pos ->
            ( translateClick model pos |> withValidMoves, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks ClickAt
        ]
