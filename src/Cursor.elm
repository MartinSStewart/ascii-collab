module Cursor exposing (Cursor, moveCursor, newLine, setCursor)

import Grid
import Helper
import Quantity exposing (Quantity)
import Units


type alias Cursor =
    { position : ( Quantity Int Units.AsciiUnit, Quantity Int Units.AsciiUnit )
    , startingColumn : Quantity Int Units.AsciiUnit
    }


moveCursor : ( Quantity Int Units.AsciiUnit, Quantity Int Units.AsciiUnit ) -> Cursor -> Cursor
moveCursor offset { position, startingColumn } =
    { position = Helper.addTuple offset position
    , startingColumn = startingColumn
    }


newLine : Cursor -> Cursor
newLine { position, startingColumn } =
    { position = ( startingColumn, Tuple.second position |> Quantity.plus (Units.asciiUnit 1) ), startingColumn = startingColumn }


setCursor : ( Quantity Int Units.AsciiUnit, Quantity Int Units.AsciiUnit ) -> Cursor
setCursor position =
    { position = position
    , startingColumn = Tuple.first position
    }
