module Evergreen.V50.LocalGrid exposing (..)

import Dict
import Evergreen.V50.Bounds
import Evergreen.V50.Grid
import Evergreen.V50.Helper
import Evergreen.V50.Units
import Evergreen.V50.User
import EverySet


type alias LocalGrid_ = 
    { grid : Evergreen.V50.Grid.Grid
    , undoHistory : (List (Dict.Dict Evergreen.V50.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V50.Helper.RawCellCoord Int))
    , user : Evergreen.V50.User.UserId
    , hiddenUsers : (EverySet.EverySet Evergreen.V50.User.UserId)
    , adminHiddenUsers : (EverySet.EverySet Evergreen.V50.User.UserId)
    , viewBounds : (Evergreen.V50.Bounds.Bounds Evergreen.V50.Units.CellUnit)
    , undoCurrent : (Dict.Dict Evergreen.V50.Helper.RawCellCoord Int)
    }


type LocalGrid
    = LocalGrid LocalGrid_