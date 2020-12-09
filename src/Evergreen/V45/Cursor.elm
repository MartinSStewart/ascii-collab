module Evergreen.V45.Cursor exposing (..)

import Evergreen.V45.Helper
import Evergreen.V45.Units
import Quantity


type Cursor
    = Cursor 
    { position : (Evergreen.V45.Helper.Coord Evergreen.V45.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V45.Units.AsciiUnit)
    , size : (Evergreen.V45.Helper.Coord Evergreen.V45.Units.AsciiUnit)
    }