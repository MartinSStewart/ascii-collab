module Evergreen.V79.Bounds exposing (..)

import Evergreen.V79.Helper


type Bounds unit
    = Bounds
        { min : Evergreen.V79.Helper.Coord unit
        , max : Evergreen.V79.Helper.Coord unit
        }
