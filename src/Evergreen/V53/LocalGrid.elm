module Evergreen.V53.LocalGrid exposing (..)

import Dict
import Evergreen.V53.Bounds
import Evergreen.V53.Grid
import Evergreen.V53.Helper
import Evergreen.V53.Units
import Evergreen.V53.User
import EverySet


type alias LocalGrid_ = 
    { grid : Evergreen.V53.Grid.Grid
    , undoHistory : (List (Dict.Dict Evergreen.V53.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V53.Helper.RawCellCoord Int))
    , user : Evergreen.V53.User.UserId
    , hiddenUsers : (EverySet.EverySet Evergreen.V53.User.UserId)
    , adminHiddenUsers : (EverySet.EverySet Evergreen.V53.User.UserId)
    , viewBounds : (Evergreen.V53.Bounds.Bounds Evergreen.V53.Units.CellUnit)
    , undoCurrent : (Dict.Dict Evergreen.V53.Helper.RawCellCoord Int)
    }


type LocalGrid
    = LocalGrid LocalGrid_