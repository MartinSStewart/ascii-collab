module Evergreen.V80.Bounds exposing (..)

import Evergreen.V80.Helper


type Bounds unit
    = Bounds
        { min : Evergreen.V80.Helper.Coord unit
        , max : Evergreen.V80.Helper.Coord unit
        }
