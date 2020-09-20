module Evergreen.V17.LocalGrid exposing (..)

import Evergreen.V17.Bounds
import Dict
import EverySet
import Evergreen.V17.Grid
import Evergreen.V17.Helper
import Evergreen.V17.Units
import Evergreen.V17.User


type alias LocalGrid_ = 
    { grid : Evergreen.V17.Grid.Grid
    , undoHistory : (List (Dict.Dict Evergreen.V17.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V17.Helper.RawCellCoord Int))
    , user : Evergreen.V17.User.UserId
    , hiddenUsers : (EverySet.EverySet Evergreen.V17.User.UserId)
    , adminHiddenUsers : (EverySet.EverySet Evergreen.V17.User.UserId)
    , viewBounds : (Evergreen.V17.Bounds.Bounds Evergreen.V17.Units.CellUnit)
    , undoCurrent : (Dict.Dict Evergreen.V17.Helper.RawCellCoord Int)
    }


type LocalGrid
    = LocalGrid LocalGrid_