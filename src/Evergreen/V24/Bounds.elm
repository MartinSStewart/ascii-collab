module Evergreen.V24.Bounds exposing (..)

import Evergreen.V24.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V24.Helper.Coord unit)
    , max : (Evergreen.V24.Helper.Coord unit)
    }