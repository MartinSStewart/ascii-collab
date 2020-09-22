module Evergreen.V24.Cursor exposing (..)

import Evergreen.V24.Helper
import Quantity
import Evergreen.V24.Units


type Cursor
    = Cursor 
    { position : (Evergreen.V24.Helper.Coord Evergreen.V24.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V24.Units.AsciiUnit)
    , size : (Evergreen.V24.Helper.Coord Evergreen.V24.Units.AsciiUnit)
    }