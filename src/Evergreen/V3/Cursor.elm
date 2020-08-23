module Evergreen.V3.Cursor exposing (..)

import Evergreen.V3.Helper
import Quantity
import Evergreen.V3.Units


type Cursor
    = Cursor 
    { position : (Evergreen.V3.Helper.Coord Evergreen.V3.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V3.Units.AsciiUnit)
    , size : (Evergreen.V3.Helper.Coord Evergreen.V3.Units.AsciiUnit)
    }