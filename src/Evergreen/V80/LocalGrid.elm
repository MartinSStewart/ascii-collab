module Evergreen.V80.LocalGrid exposing (..)

import Dict
import Evergreen.V80.Bounds
import Evergreen.V80.Grid
import Evergreen.V80.Helper
import Evergreen.V80.Units
import Evergreen.V80.User
import EverySet


type alias LocalGrid_ =
    { grid : Evergreen.V80.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V80.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V80.Helper.RawCellCoord Int)
    , user : Evergreen.V80.User.UserId
    , hiddenUsers : EverySet.EverySet Evergreen.V80.User.UserId
    , adminHiddenUsers : EverySet.EverySet Evergreen.V80.User.UserId
    , viewBounds : Evergreen.V80.Bounds.Bounds Evergreen.V80.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V80.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
