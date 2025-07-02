module Evergreen.V81.Bounds exposing (..)

import Evergreen.V81.Helper


type Bounds unit
    = Bounds
        { min : Evergreen.V81.Helper.Coord unit
        , max : Evergreen.V81.Helper.Coord unit
        }
