module Evergreen.V50.Cursor exposing (..)

import Evergreen.V50.Helper
import Evergreen.V50.Units
import Quantity


type Cursor
    = Cursor 
    { position : (Evergreen.V50.Helper.Coord Evergreen.V50.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V50.Units.AsciiUnit)
    , size : (Evergreen.V50.Helper.Coord Evergreen.V50.Units.AsciiUnit)
    }