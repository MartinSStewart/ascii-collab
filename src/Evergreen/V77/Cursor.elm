module Evergreen.V77.Cursor exposing (..)

import Evergreen.V77.Helper
import Evergreen.V77.Units
import Quantity


type Cursor
    = Cursor
        { position : Evergreen.V77.Helper.Coord Evergreen.V77.Units.AsciiUnit
        , startingColumn : Quantity.Quantity Int Evergreen.V77.Units.AsciiUnit
        , size : Evergreen.V77.Helper.Coord Evergreen.V77.Units.AsciiUnit
        }
