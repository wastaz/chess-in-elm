import Html exposing (text)
import Html.App as App
import Collage as GC
import Collage exposing (defaultLine)
import Element exposing (toHtml, image)
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
    List.concat
        [
            List.map (\i -> (White, Pawn, (i, 1))) [0 .. 7],
            List.map (\i -> (Black, Pawn, (i, 6))) [0 .. 7],
            [ (White, 0), (Black, 7)]
            |> List.map (\(clr, y) ->
                    [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
                    |> List.indexedMap (\x pc -> (clr, pc, (x, y)))
                )
            |> List.concat
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
        Black -> Color.charcoal

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

drawBoard clr =
    [0..7]
    |> List.map (\i ->
            let startColor = if i % 2 == 0 then clr else other clr
            in
                makeRow startColor
                |> GC.toForm
                |> GC.moveY ((toFloat i) * 100.0 - 350.0)
        )
    |> GC.collage 800 800

positionToCoords (x, y) =
    let
        cx = (toFloat x) * 100.0 - 350.0
        cy = (toFloat y) * 100.0 - 350.0
    in
        (cx, cy)

imageFor player piece =
    let
        playerstr =
            case player of
                Black -> "black"
                White -> "white"
        piecestr =
            case piece of
                Pawn -> "pawn"
                Rook -> "rook"
                Knight -> "knight"
                Bishop -> "bishop"
                Queen -> "queen"
                King -> "king"
    in
        "assets/" ++ piecestr ++ "_" ++ playerstr ++ ".png"

drawPieces pcs =
    pcs
    |> List.map (\(player, piece, position) ->
            imageFor player piece
            |> image 50 50
            |> GC.toForm
            |> GC.move (positionToCoords position)
        )

view model =
    List.concat
        [
            [
                GC.rect 805 805 |> GC.outlined { defaultLine | width = 5 },
                drawBoard Black |> GC.toForm
            ],
            drawPieces model
        ]

    |> GC.collage 810 810
    |> toHtml
