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
      , markedSquares = []
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


bind : (a -> Maybe b) -> Maybe a -> Maybe b
bind fn o =
    Maybe.andThen o fn


translateClickOnEmptySquare : Model -> BoardPosition -> Model
translateClickOnEmptySquare model pos =
    case model.activePiece of
        Nothing ->
            { model | markedSquares = [], activePiece = Nothing }

        Just ( pl, pc, ps ) ->
            let
                moveResult =
                    move model.board ( pl, pc, ps ) pos
            in
                case moveResult of
                    Nothing ->
                        model

                    Just b ->
                        { model | board = b, activePiece = Nothing, activePlayer = other model.activePlayer, markedSquares = [] }


translateClickOnPieceSquare : Model -> BoardPosition -> Model
translateClickOnPieceSquare model pos =
    case pieceAt model.board pos of
        Nothing ->
            translateClickOnEmptySquare model pos

        Just p ->
            { model | markedSquares = validMoves p, activePiece = Just p }


translateClick : Model -> Position -> Model
translateClick model pos =
    pos
        |> positionToCoord
        |> Maybe.map (translateClickOnPieceSquare model)
        |> Maybe.withDefault { model | markedSquares = [], activePiece = Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ClickAt pos ->
            ( translateClick model pos, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks ClickAt
        ]
