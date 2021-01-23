module Evergreen.V53.Cursor exposing (..)

import Evergreen.V53.Helper
import Evergreen.V53.Units
import Quantity


type Cursor
    = Cursor 
    { position : (Evergreen.V53.Helper.Coord Evergreen.V53.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V53.Units.AsciiUnit)
    , size : (Evergreen.V53.Helper.Coord Evergreen.V53.Units.AsciiUnit)
    }