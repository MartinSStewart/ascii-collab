module Evergreen.V85.Bounds exposing (..)

import Evergreen.V85.Helper


type Bounds unit
    = Bounds
        { min : Evergreen.V85.Helper.Coord unit
        , max : Evergreen.V85.Helper.Coord unit
        }
