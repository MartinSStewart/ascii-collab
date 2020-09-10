module Evergreen.V8.Cursor exposing (..)

import Evergreen.V8.Helper
import Quantity
import Evergreen.V8.Units


type Cursor
    = Cursor 
    { position : (Evergreen.V8.Helper.Coord Evergreen.V8.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V8.Units.AsciiUnit)
    , size : (Evergreen.V8.Helper.Coord Evergreen.V8.Units.AsciiUnit)
    }