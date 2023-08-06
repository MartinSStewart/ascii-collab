module Evergreen.V79.Cursor exposing (..)

import Evergreen.V79.Helper
import Evergreen.V79.Units
import Quantity


type Cursor
    = Cursor
        { position : Evergreen.V79.Helper.Coord Evergreen.V79.Units.AsciiUnit
        , startingColumn : Quantity.Quantity Int Evergreen.V79.Units.AsciiUnit
        , size : Evergreen.V79.Helper.Coord Evergreen.V79.Units.AsciiUnit
        }
