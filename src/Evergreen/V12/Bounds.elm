module Evergreen.V12.Bounds exposing (..)

import Evergreen.V12.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V12.Helper.Coord unit)
    , max : (Evergreen.V12.Helper.Coord unit)
    }