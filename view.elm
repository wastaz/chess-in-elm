module View exposing (view)

import Collage exposing (defaultLine, rect, filled, alpha, outlined, moveX, moveY, move, collage, toForm)
import Html as Html
import Html.Attributes as Attr
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


drawPieces : List Piece -> List Collage.Form
drawPieces pcs =
    pcs
        |> List.map
            (\piece ->
                imageFor piece.owner piece.kind
                    |> image 50 50
                    |> toForm
                    |> move (positionToCoords piece.position)
            )


markSquareWithColor : Color.Color -> BoardPosition -> Collage.Form
markSquareWithColor clr pos =
    rect 100 100
        |> filled clr
        |> alpha 0.5
        |> move (positionToCoords pos)


drawMarkedSquare : Move -> Collage.Form
drawMarkedSquare move =
    let
        color =
            case move.moveType of
                Normal ->
                    Color.blue

                Attack ->
                    Color.red
    in
        markSquareWithColor color move.position


drawActivePieceSquare : BoardPosition -> Collage.Form
drawActivePieceSquare =
    markSquareWithColor Color.yellow


viewBoard : Model -> Html.Html Msg
viewBoard model =
    List.concat
        [ [ rect 805 805 |> outlined { defaultLine | width = 5 }
          , drawBoard Black |> toForm
          ]
        , drawPieces model.board
        , List.map drawMarkedSquare model.possibleMoves
        , [ model.activePiece |> Maybe.map (.position >> drawActivePieceSquare) ] |> List.filterMap identity
        ]
        |> collage 810 810
        |> toHtml


playerToString : Player -> String
playerToString player =
    case player of
        White ->
            "White"

        Black ->
            "Black"


viewSidebar : Model -> Html.Html Msg
viewSidebar model =
    Html.div [ Attr.style [ ( "border", "5px solid black" ), ( "padding", "5px" ), ( "text-align", "center" ) ] ]
        [ Html.div []
            [ Html.h1 [] [ Html.text "Current player" ]
            , Html.p [] [ Html.text <| playerToString model.activePlayer ]
            ]
        ]


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div [ Attr.style [ ( "float", "left" ) ] ] [ viewBoard model ]
        , Html.div [ Attr.style [ ( "float", "left" ), ( "margin-left", "50px" ) ] ] [ viewSidebar model ]
        ]
