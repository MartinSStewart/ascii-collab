module Evergreen.V28.Bounds exposing (..)

import Evergreen.V28.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V28.Helper.Coord unit)
    , max : (Evergreen.V28.Helper.Coord unit)
    }