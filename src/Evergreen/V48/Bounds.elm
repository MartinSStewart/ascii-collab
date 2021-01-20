module Evergreen.V48.Bounds exposing (..)

import Evergreen.V48.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V48.Helper.Coord unit)
    , max : (Evergreen.V48.Helper.Coord unit)
    }