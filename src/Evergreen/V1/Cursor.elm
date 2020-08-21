module Evergreen.V1.Cursor exposing (..)

import Evergreen.V1.Helper
import Quantity
import Evergreen.V1.Units


type Cursor
    = Cursor 
    { position : (Evergreen.V1.Helper.Coord Evergreen.V1.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V1.Units.AsciiUnit)
    , size : (Evergreen.V1.Helper.Coord Evergreen.V1.Units.AsciiUnit)
    }