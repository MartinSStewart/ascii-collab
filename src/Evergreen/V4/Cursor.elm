module Evergreen.V4.Cursor exposing (..)

import Evergreen.V4.Helper
import Quantity
import Evergreen.V4.Units


type Cursor
    = Cursor 
    { position : (Evergreen.V4.Helper.Coord Evergreen.V4.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V4.Units.AsciiUnit)
    , size : (Evergreen.V4.Helper.Coord Evergreen.V4.Units.AsciiUnit)
    }