module View exposing (view)

import Collage exposing (defaultLine, rect, filled, alpha, outlined, moveX, moveY, move, collage, toForm)
import Html as Html
import Element exposing (toHtml, image)
import Color as Color
import ChessTypes exposing (..)


toColor : Player -> Color.Color
toColor player =
    case player of
        White ->
            Color.white

        Black ->
            Color.charcoal


makeBasicSquare : Int -> Color.Color -> Collage.Form
makeBasicSquare i cellColor =
    rect 100 100
        |> filled cellColor
        |> moveX ((toFloat i) * 100.0 - 350.0)


idxToColor : Player -> Int -> Color.Color
idxToColor clr i =
    (if i % 2 == 0 then
        clr
     else
        other clr
    )
        |> toColor


makeRow : Player -> Element.Element
makeRow clr =
    [0..7]
        |> List.map (\i -> makeBasicSquare i <| idxToColor clr i)
        |> collage 800 100


drawBoard : Player -> Element.Element
drawBoard clr =
    [0..7]
        |> List.map
            (\i ->
                let
                    startColor =
                        if i % 2 == 0 then
                            clr
                        else
                            other clr
                in
                    makeRow startColor
                        |> toForm
                        |> moveY ((toFloat i) * 100.0 - 350.0)
            )
        |> collage 800 800


positionToCoords : BoardPosition -> ( Float, Float )
positionToCoords bp =
    let
        cx =
            (toFloat bp.x) * 100.0 - 350.0

        cy =
            (toFloat bp.y) * 100.0 - 350.0
    in
        ( cx, cy )


imageFor : Player -> ChessPiece -> String
imageFor player piece =
    let
        playerstr =
            case player of
                Black ->
                    "black"

                White ->
                    "white"

        piecestr =
            case piece of
                Pawn ->
                    "pawn"

                Rook ->
                    "rook"

                Knight ->
                    "knight"

                Bishop ->
                    "bishop"

                Queen ->
                    "queen"

                King ->
                    "king"
    in
        "assets/" ++ piecestr ++ "_" ++ playerstr ++ ".png"


drawPieces : List ( Player, ChessPiece, BoardPosition ) -> List Collage.Form
drawPieces pcs =
    pcs
        |> List.map
            (\( player, piece, position ) ->
                imageFor player piece
                    |> image 50 50
                    |> toForm
                    |> move (positionToCoords position)
            )


markSquareWithColor : Color.Color -> BoardPosition -> Collage.Form
markSquareWithColor clr pos =
    rect 100 100
        |> filled clr
        |> alpha 0.5
        |> move (positionToCoords pos)


drawMarkedSquare : BoardPosition -> Collage.Form
drawMarkedSquare =
    markSquareWithColor Color.blue


drawActivePieceSquare : BoardPosition -> Collage.Form
drawActivePieceSquare =
    markSquareWithColor Color.yellow


view : Model -> Html.Html Msg
view model =
    List.concat
        [ [ rect 805 805 |> outlined { defaultLine | width = 5 }
          , drawBoard Black |> toForm
          ]
        , drawPieces model.board
        , List.map drawMarkedSquare model.markedSquares
        , [ model.activePiece |> Maybe.map (\(_, _, c) -> drawActivePieceSquare c) ] |> List.filterMap identity
        ]
        |> collage 810 810
        |> toHtml
