module Evergreen.V14.Cursor exposing (..)

import Evergreen.V14.Helper
import Quantity
import Evergreen.V14.Units


type Cursor
    = Cursor 
    { position : (Evergreen.V14.Helper.Coord Evergreen.V14.Units.AsciiUnit)
    , startingColumn : (Quantity.Quantity Int Evergreen.V14.Units.AsciiUnit)
    , size : (Evergreen.V14.Helper.Coord Evergreen.V14.Units.AsciiUnit)
    }