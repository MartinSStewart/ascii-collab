module Evergreen.V8.Bounds exposing (..)

import Evergreen.V8.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V8.Helper.Coord unit)
    , max : (Evergreen.V8.Helper.Coord unit)
    }