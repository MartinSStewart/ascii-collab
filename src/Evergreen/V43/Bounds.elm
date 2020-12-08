module Evergreen.V43.Bounds exposing (..)

import Evergreen.V43.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V43.Helper.Coord unit)
    , max : (Evergreen.V43.Helper.Coord unit)
    }