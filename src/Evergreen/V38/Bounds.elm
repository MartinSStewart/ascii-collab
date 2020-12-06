module Evergreen.V38.Bounds exposing (..)

import Evergreen.V38.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V38.Helper.Coord unit)
    , max : (Evergreen.V38.Helper.Coord unit)
    }