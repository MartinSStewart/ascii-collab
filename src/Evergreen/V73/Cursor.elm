module Evergreen.V73.Cursor exposing (..)

import Evergreen.V73.Helper
import Evergreen.V73.Units
import Quantity


type Cursor
    = Cursor
        { position : Evergreen.V73.Helper.Coord Evergreen.V73.Units.AsciiUnit
        , startingColumn : Quantity.Quantity Int Evergreen.V73.Units.AsciiUnit
        , size : Evergreen.V73.Helper.Coord Evergreen.V73.Units.AsciiUnit
        }
