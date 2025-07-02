module Evergreen.V81.Cursor exposing (..)

import Evergreen.V81.Helper
import Evergreen.V81.Units
import Quantity


type Cursor
    = Cursor
        { position : Evergreen.V81.Helper.Coord Evergreen.V81.Units.AsciiUnit
        , startingColumn : Quantity.Quantity Int Evergreen.V81.Units.AsciiUnit
        , size : Evergreen.V81.Helper.Coord Evergreen.V81.Units.AsciiUnit
        }
