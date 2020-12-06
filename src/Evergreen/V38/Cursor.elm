module Evergreen.V38.Cursor exposing (..)

import Evergreen.V38.Helper
import Evergreen.V38.Units
import Quantity


type Cursor
    = Cursor 
    { position : (Evergreen.V38.Helper.Coord Evergreen.V38.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V38.Units.AsciiUnit)
    , size : (Evergreen.V38.Helper.Coord Evergreen.V38.Units.AsciiUnit)
    }