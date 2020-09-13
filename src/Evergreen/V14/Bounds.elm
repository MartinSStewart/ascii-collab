module Evergreen.V14.Bounds exposing (..)

import Evergreen.V14.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V14.Helper.Coord unit)
    , max : (Evergreen.V14.Helper.Coord unit)
    }