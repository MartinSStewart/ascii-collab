module Evergreen.V12.LocalGrid exposing (..)

import Evergreen.V12.Bounds
import Dict
import EverySet
import Evergreen.V12.Grid
import Evergreen.V12.Helper
import Evergreen.V12.Units
import Evergreen.V12.User


type alias LocalGrid_ = 
    { grid : Evergreen.V12.Grid.Grid
    , undoHistory : (List (Dict.Dict Evergreen.V12.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V12.Helper.RawCellCoord Int))
    , user : (Evergreen.V12.User.UserId, Evergreen.V12.User.UserData)
    , otherUsers : (List (Evergreen.V12.User.UserId, Evergreen.V12.User.UserData))
    , hiddenUsers : (EverySet.EverySet Evergreen.V12.User.UserId)
    , adminHiddenUsers : (EverySet.EverySet Evergreen.V12.User.UserId)
    , viewBounds : (Evergreen.V12.Bounds.Bounds Evergreen.V12.Units.CellUnit)
    , undoCurrent : (Dict.Dict Evergreen.V12.Helper.RawCellCoord Int)
    }


type LocalGrid
    = LocalGrid LocalGrid_