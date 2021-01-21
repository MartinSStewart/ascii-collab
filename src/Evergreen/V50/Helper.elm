module Evergreen.V50.Helper exposing (..)

import Quantity


type alias Coord units = ((Quantity.Quantity Int units), (Quantity.Quantity Int units))


type alias RawCellCoord = (Int, Int)