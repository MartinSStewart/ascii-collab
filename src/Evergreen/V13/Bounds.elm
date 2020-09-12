module Evergreen.V13.Bounds exposing (..)

import Evergreen.V13.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V13.Helper.Coord unit)
    , max : (Evergreen.V13.Helper.Coord unit)
    }