module Evergreen.V77.LocalGrid exposing (..)

import Dict
import Evergreen.V77.Bounds
import Evergreen.V77.Grid
import Evergreen.V77.Helper
import Evergreen.V77.Units
import Evergreen.V77.User
import EverySet


type alias LocalGrid_ =
    { grid : Evergreen.V77.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V77.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V77.Helper.RawCellCoord Int)
    , user : Evergreen.V77.User.UserId
    , hiddenUsers : EverySet.EverySet Evergreen.V77.User.UserId
    , adminHiddenUsers : EverySet.EverySet Evergreen.V77.User.UserId
    , viewBounds : Evergreen.V77.Bounds.Bounds Evergreen.V77.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V77.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
