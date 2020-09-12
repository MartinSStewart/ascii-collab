module Evergreen.V12.Cursor exposing (..)

import Evergreen.V12.Helper
import Quantity
import Evergreen.V12.Units


type Cursor
    = Cursor 
    { position : (Evergreen.V12.Helper.Coord Evergreen.V12.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V12.Units.AsciiUnit)
    , size : (Evergreen.V12.Helper.Coord Evergreen.V12.Units.AsciiUnit)
    }