module Evergreen.V83.Cursor exposing (..)

import Evergreen.V83.Helper
import Evergreen.V83.Units
import Quantity


type Cursor
    = Cursor
        { position : Evergreen.V83.Helper.Coord Evergreen.V83.Units.AsciiUnit
        , startingColumn : Quantity.Quantity Int Evergreen.V83.Units.AsciiUnit
        , size : Evergreen.V83.Helper.Coord Evergreen.V83.Units.AsciiUnit
        }
