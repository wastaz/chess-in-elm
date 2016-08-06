import Html exposing (text)
import Html.App as App
import Collage as GC
import Collage exposing (defaultLine)
import Element exposing (toHtml)
import Color as Color

type ChessPiece
    = Pawn
    | Rook
    | Knight
    | Bishop
    | Queen
    | King

type Player
    = Black
    | White

type alias Position = (Int, Int)

type alias Piece = (Player, ChessPiece, Position)

initialBoard =
    [ (Black, Pawn, (1, 2)),
      (Black, Pawn, (2, 2))
    ]

main =
    App.beginnerProgram { model = initialBoard, view = view, update = update }

type Msg = Noop

update msg model =
    model

other player =
    case player of
        White -> Black
        Black -> White

toColor player =
    case player of
        White -> Color.white
        Black -> Color.black

makeRow clr =
    [0..7]
    |> List.map (\i ->
        let cellColor =
            (if i % 2 == 0 then clr else other clr)
            |> toColor
        in
            GC.rect 100 100
            |> GC.filled cellColor
            |> GC.moveX ((toFloat i) * 100.0 - 350.0)
        )
    |> GC.collage 800 100

makeBoard clr =
    [0..7]
    |> List.map (\i ->
            let startColor = if i % 2 == 0 then clr else other clr
            in
                makeRow startColor
                |> GC.toForm
                |> GC.moveY ((toFloat i) * 100.0 - 350.0)
        )
    |> GC.collage 800 800

drawBoard =
    [
        GC.rect 805 805 |> GC.outlined { defaultLine | width = 5 },
        makeBoard White |> GC.toForm
    ]
    |> GC.collage 810 810


view model =
    drawBoard
    |> toHtml
