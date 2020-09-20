module Evergreen.V17.Bounds exposing (..)

import Evergreen.V17.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V17.Helper.Coord unit)
    , max : (Evergreen.V17.Helper.Coord unit)
    }