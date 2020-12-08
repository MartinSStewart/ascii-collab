module Evergreen.V43.Cursor exposing (..)

import Evergreen.V43.Helper
import Evergreen.V43.Units
import Quantity


type Cursor
    = Cursor 
    { position : (Evergreen.V43.Helper.Coord Evergreen.V43.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V43.Units.AsciiUnit)
    , size : (Evergreen.V43.Helper.Coord Evergreen.V43.Units.AsciiUnit)
    }