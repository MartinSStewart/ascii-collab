module Evergreen.V10.Bounds exposing (..)

import Evergreen.V10.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V10.Helper.Coord unit)
    , max : (Evergreen.V10.Helper.Coord unit)
    }