module Evergreen.V77.Bounds exposing (..)

import Evergreen.V77.Helper


type Bounds unit
    = Bounds
        { min : Evergreen.V77.Helper.Coord unit
        , max : Evergreen.V77.Helper.Coord unit
        }
