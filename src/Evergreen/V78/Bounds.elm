module Evergreen.V78.Bounds exposing (..)

import Evergreen.V78.Helper


type Bounds unit
    = Bounds
        { min : Evergreen.V78.Helper.Coord unit
        , max : Evergreen.V78.Helper.Coord unit
        }
