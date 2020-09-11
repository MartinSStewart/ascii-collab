module Evergreen.V10.Cursor exposing (..)

import Evergreen.V10.Helper
import Quantity
import Evergreen.V10.Units


type Cursor
    = Cursor 
    { position : (Evergreen.V10.Helper.Coord Evergreen.V10.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V10.Units.AsciiUnit)
    , size : (Evergreen.V10.Helper.Coord Evergreen.V10.Units.AsciiUnit)
    }