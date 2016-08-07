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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Noop ->
            (model, Cmd.none)
        ClickAt pos ->
            case positionToCoord pos of
                Nothing -> ({ model | markedSquares = []}, Cmd.none)
                Just c -> ({ model | markedSquares = [c]}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks ClickAt
        ]
