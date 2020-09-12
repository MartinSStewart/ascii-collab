module Evergreen.V13.Cursor exposing (..)

import Evergreen.V13.Helper
import Quantity
import Evergreen.V13.Units


type Cursor
    = Cursor 
    { position : (Evergreen.V13.Helper.Coord Evergreen.V13.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V13.Units.AsciiUnit)
    , size : (Evergreen.V13.Helper.Coord Evergreen.V13.Units.AsciiUnit)
    }