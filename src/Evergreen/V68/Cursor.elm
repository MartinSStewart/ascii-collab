module Evergreen.V68.Cursor exposing (..)

import Evergreen.V68.Helper
import Evergreen.V68.Units
import Quantity


type Cursor
    = Cursor
        { position : Evergreen.V68.Helper.Coord Evergreen.V68.Units.AsciiUnit
        , startingColumn : Quantity.Quantity Int Evergreen.V68.Units.AsciiUnit
        , size : Evergreen.V68.Helper.Coord Evergreen.V68.Units.AsciiUnit
        }
