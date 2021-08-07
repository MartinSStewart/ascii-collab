module Evergreen.V73.Bounds exposing (..)

import Evergreen.V73.Helper


type Bounds unit
    = Bounds
        { min : Evergreen.V73.Helper.Coord unit
        , max : Evergreen.V73.Helper.Coord unit
        }
