module Evergreen.V68.Bounds exposing (..)

import Evergreen.V68.Helper


type Bounds unit
    = Bounds
        { min : Evergreen.V68.Helper.Coord unit
        , max : Evergreen.V68.Helper.Coord unit
        }
