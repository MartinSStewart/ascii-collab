module GridCell exposing (Cell, addLine, addLineWithChangeId, cellSize, changeCount, empty, flatten)

import Array exposing (Array)
import Ascii exposing (Ascii)
import List.Nonempty exposing (Nonempty)
import User exposing (UserId)


type Cell
    = Cell (List { userId : UserId, position : Int, line : Nonempty Ascii })


addLine : UserId -> Int -> Nonempty Ascii -> Cell -> Cell
addLine userId position line (Cell history) =
    { userId = userId, position = position, line = line } :: history |> Cell


addLineWithChangeId : Int -> UserId -> Int -> Nonempty Ascii -> Cell -> Cell
addLineWithChangeId changeId userId position line (Cell history) =
    List.take changeId (List.reverse history)
        |> List.reverse
        |> (::) { userId = userId, position = position, line = line }
        |> Cell


changeCount : Cell -> Int
changeCount (Cell cell) =
    List.length cell


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
        (Array.initialize (cellSize * cellSize) (\_ -> Ascii.default))
        history


cellSize : Int
cellSize =
    16


empty : Cell
empty =
    Cell []
