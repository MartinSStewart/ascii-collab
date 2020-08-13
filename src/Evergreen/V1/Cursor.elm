module Evergreen.V1.Cursor exposing (..)

import Quantity
import Evergreen.V1.Units


type alias Cursor = 
    { position : ((Quantity.Quantity Int Evergreen.V1.Units.AsciiUnit), (Quantity.Quantity Int Evergreen.V1.Units.AsciiUnit))
    , startingColumn : (Quantity.Quantity Int Evergreen.V1.Units.AsciiUnit)
    }