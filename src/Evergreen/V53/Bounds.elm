module Evergreen.V53.Bounds exposing (..)

import Evergreen.V53.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V53.Helper.Coord unit)
    , max : (Evergreen.V53.Helper.Coord unit)
    }