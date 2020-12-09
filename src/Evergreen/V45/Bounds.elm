module Evergreen.V45.Bounds exposing (..)

import Evergreen.V45.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V45.Helper.Coord unit)
    , max : (Evergreen.V45.Helper.Coord unit)
    }