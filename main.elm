import Html exposing (text)
import Html.App as App
import Collage as GC
import Collage exposing (defaultLine)
import Element exposing (toHtml, image)
import Mouse exposing (Position)
import Color as Color
import ChessTypes exposing (..)
import View exposing (view)
import Moves exposing (pieceAt)

main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : (Model, Cmd Msg)
init =
    ({ board = initialBoard
     , markedSquares = []
     , activePlayer = White
     }, Cmd.none)

positionToCoord : Position -> Maybe BoardPosition
positionToCoord pos =
    let
        x = (pos.x - 5) // 100
        y = (pos.y - 5) // 100
    in
        if x > 7 || y > 7 then Nothing else Just { x = x, y = 7 - y}

bind : (a -> Maybe b) -> Maybe a -> Maybe b
bind fn o =
    Maybe.andThen o fn

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Noop ->
            (model, Cmd.none)
        ClickAt pos ->
            pos
            |> positionToCoord
            |> bind (pieceAt model.board)
            |> Maybe.map (\(_, _, c) -> { model | markedSquares = [c]})
            |> Maybe.withDefault { model | markedSquares = []}
            |> \m -> (m, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks ClickAt
        ]
