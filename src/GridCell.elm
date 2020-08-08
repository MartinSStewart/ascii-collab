module GridCell exposing (Cell, addChange, cellSize, empty, flatten)

import Array exposing (Array)
import Ascii exposing (Ascii)
import Dict
import Math.Vector2 exposing (Vec2)
import Quantity exposing (Quantity(..))
import User exposing (UserId)
import WebGL


type Cell
    = Cell (List { userId : UserId, position : Int, ascii : Ascii })


addChange : UserId -> Int -> Ascii -> Cell -> Cell
addChange userId position ascii (Cell history) =
    { userId = userId, position = position, ascii = ascii } :: history |> Cell


flatten : Cell -> Array Ascii
flatten (Cell history) =
    List.foldl
        (\{ userId, position, ascii } state ->
            Array.set position ascii state
        )
        (Array.initialize 255 (\_ -> Ascii.default))
        history


cellSize : Int
cellSize =
    16


empty : Cell
empty =
    Cell []
