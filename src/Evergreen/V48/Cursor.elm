module Evergreen.V48.Cursor exposing (..)

import Evergreen.V48.Helper
import Evergreen.V48.Units
import Quantity


type Cursor
    = Cursor 
    { position : (Evergreen.V48.Helper.Coord Evergreen.V48.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V48.Units.AsciiUnit)
    , size : (Evergreen.V48.Helper.Coord Evergreen.V48.Units.AsciiUnit)
    }