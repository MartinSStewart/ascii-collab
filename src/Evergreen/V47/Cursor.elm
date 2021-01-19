module Evergreen.V47.Cursor exposing (..)

import Evergreen.V47.Helper
import Evergreen.V47.Units
import Quantity


type Cursor
    = Cursor 
    { position : (Evergreen.V47.Helper.Coord Evergreen.V47.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V47.Units.AsciiUnit)
    , size : (Evergreen.V47.Helper.Coord Evergreen.V47.Units.AsciiUnit)
    }