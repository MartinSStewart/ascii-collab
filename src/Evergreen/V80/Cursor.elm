module Evergreen.V80.Cursor exposing (..)

import Evergreen.V80.Helper
import Evergreen.V80.Units
import Quantity


type Cursor
    = Cursor
        { position : Evergreen.V80.Helper.Coord Evergreen.V80.Units.AsciiUnit
        , startingColumn : Quantity.Quantity Int Evergreen.V80.Units.AsciiUnit
        , size : Evergreen.V80.Helper.Coord Evergreen.V80.Units.AsciiUnit
        }
