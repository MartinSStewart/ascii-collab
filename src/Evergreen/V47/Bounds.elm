module Evergreen.V47.Bounds exposing (..)

import Evergreen.V47.Helper


type Bounds unit
    = Bounds 
    { min : (Evergreen.V47.Helper.Coord unit)
    , max : (Evergreen.V47.Helper.Coord unit)
    }