module Main exposing (..)

import Html.App as App
import Mouse exposing (Position)
import ChessTypes exposing (..)
import View exposing (view)
import Moves exposing (pieceAt, validMoves)


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


translateClick : Model -> Position -> Model
translateClick model pos =
    pos
        |> positionToCoord
        |> bind (pieceAt model.board)
        |> Maybe.map (\p -> { model | markedSquares = validMoves p, activePiece = Just p })
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
