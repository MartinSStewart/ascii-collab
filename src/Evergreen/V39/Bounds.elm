module Evergreen.V39.Bounds exposing (..)

import Evergreen.V39.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V39.Helper.Coord unit)
    , max : (Evergreen.V39.Helper.Coord unit)
    }