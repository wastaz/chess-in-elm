import Html exposing (text)
import Html.App as App
import Collage as GC
import Collage exposing (defaultLine)
import Element exposing (toHtml, image)
import Mouse exposing (Position)
import Color as Color
import ChessTypes exposing (..)
import View exposing (view)

main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : (Model, Cmd Msg)
init =
    ({ board = initialBoard, markedSquare = Nothing }, Cmd.none)

positionToCoord : Position -> Maybe (Int, Int)
positionToCoord pos =
    let
        x = (pos.x - 5) // 100
        y = (pos.y - 5) // 100
    in
        if x > 7 || y > 7 then Nothing else Just (x, 7 - y)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Noop ->
            (model, Cmd.none)
        ClickAt pos ->
            ({ model | markedSquare = positionToCoord pos }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks ClickAt
        ]
