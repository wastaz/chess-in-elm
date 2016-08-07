module View exposing (view)

import Collage exposing (defaultLine, rect, filled, alpha, outlined, moveX, moveY, move, collage, toForm)
import Html as Html
import Element exposing (toHtml, image)
import Color as Color
import ChessTypes exposing (..)

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
            rect 100 100
            |> filled cellColor
            |> moveX ((toFloat i) * 100.0 - 350.0)
        )
    |> collage 800 100

drawBoard clr =
    [0..7]
    |> List.map (\i ->
            let startColor = if i % 2 == 0 then clr else other clr
            in
                makeRow startColor
                |> toForm
                |> moveY ((toFloat i) * 100.0 - 350.0)
        )
    |> collage 800 800

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
            |> toForm
            |> move (positionToCoords position)
        )

drawMarkedSquare : Maybe Coord -> List Collage.Form
drawMarkedSquare square =
    case square of
        Nothing -> []
        Just pos ->
            [
                rect 100 100
                |> filled Color.blue
                |> alpha 0.5
                |> move (positionToCoords pos)
            ]

view : Model -> Html.Html Msg
view model =
    List.concat
        [
            [
                rect 805 805 |> outlined { defaultLine | width = 5 },
                drawBoard Black |> toForm
            ],
            drawPieces model.board,
            drawMarkedSquare model.markedSquare
        ]

    |> collage 810 810
    |> toHtml
