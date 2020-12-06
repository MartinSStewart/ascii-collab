module Evergreen.V39.Cursor exposing (..)

import Evergreen.V39.Helper
import Evergreen.V39.Units
import Quantity


type Cursor
    = Cursor 
    { position : (Evergreen.V39.Helper.Coord Evergreen.V39.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V39.Units.AsciiUnit)
    , size : (Evergreen.V39.Helper.Coord Evergreen.V39.Units.AsciiUnit)
    }