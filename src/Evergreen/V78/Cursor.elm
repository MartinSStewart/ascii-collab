module Evergreen.V78.Cursor exposing (..)

import Evergreen.V78.Helper
import Evergreen.V78.Units
import Quantity


type Cursor
    = Cursor
        { position : Evergreen.V78.Helper.Coord Evergreen.V78.Units.AsciiUnit
        , startingColumn : Quantity.Quantity Int Evergreen.V78.Units.AsciiUnit
        , size : Evergreen.V78.Helper.Coord Evergreen.V78.Units.AsciiUnit
        }
