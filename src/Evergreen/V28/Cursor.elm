module Evergreen.V28.Cursor exposing (..)

import Evergreen.V28.Helper
import Quantity
import Evergreen.V28.Units


type Cursor
    = Cursor 
    { position : (Evergreen.V28.Helper.Coord Evergreen.V28.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V28.Units.AsciiUnit)
    , size : (Evergreen.V28.Helper.Coord Evergreen.V28.Units.AsciiUnit)
    }