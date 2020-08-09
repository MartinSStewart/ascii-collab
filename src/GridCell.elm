module GridCell exposing (Cell, addLine, cellSize, empty, flatten)

import Array exposing (Array)
import Ascii exposing (Ascii)
import List.Nonempty exposing (Nonempty)
import User exposing (UserId)


type Cell
    = Cell (List { userId : UserId, position : Int, line : Nonempty Ascii })


addLine : UserId -> Int -> Nonempty Ascii -> Cell -> Cell
addLine userId position line (Cell history) =
    { userId = userId, position = position, line = line } :: history |> Cell


flatten : Cell -> Array Ascii
flatten (Cell history) =
    List.foldr
        (\{ userId, position, line } state ->
            List.Nonempty.foldl
                (\ascii ( position_, state_ ) -> ( position_ + 1, Array.set position_ ascii state_ ))
                ( position, state )
                line
                |> Tuple.second
        )
        (Array.initialize 255 (\_ -> Ascii.default))
        history


cellSize : Int
cellSize =
    16


empty : Cell
empty =
    Cell []
