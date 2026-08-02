module Evergreen.V85.Cursor exposing (..)

import Evergreen.V85.Helper
import Evergreen.V85.Units
import Quantity


type Cursor
    = Cursor
        { position : Evergreen.V85.Helper.Coord Evergreen.V85.Units.AsciiUnit
        , startingColumn : Quantity.Quantity Int Evergreen.V85.Units.AsciiUnit
        , size : Evergreen.V85.Helper.Coord Evergreen.V85.Units.AsciiUnit
        }
