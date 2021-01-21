module Evergreen.V50.Bounds exposing (..)

import Evergreen.V50.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V50.Helper.Coord unit)
    , max : (Evergreen.V50.Helper.Coord unit)
    }