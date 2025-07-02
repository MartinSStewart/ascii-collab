module Evergreen.V83.Bounds exposing (..)

import Evergreen.V83.Helper


type Bounds unit
    = Bounds
        { min : Evergreen.V83.Helper.Coord unit
        , max : Evergreen.V83.Helper.Coord unit
        }
