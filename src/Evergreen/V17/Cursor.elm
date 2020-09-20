module Evergreen.V17.Cursor exposing (..)

import Evergreen.V17.Helper
import Quantity
import Evergreen.V17.Units


type Cursor
    = Cursor 
    { position : (Evergreen.V17.Helper.Coord Evergreen.V17.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V17.Units.AsciiUnit)
    , size : (Evergreen.V17.Helper.Coord Evergreen.V17.Units.AsciiUnit)
    }