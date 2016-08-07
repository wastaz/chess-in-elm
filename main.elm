import Html exposing (text)
import Html.App as App
import Collage as GC
import Collage exposing (defaultLine)
import Element exposing (toHtml, image)
import Color as Color
import ChessTypes exposing (..)
import View exposing (view)

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
