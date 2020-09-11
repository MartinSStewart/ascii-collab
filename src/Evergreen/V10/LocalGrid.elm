module Evergreen.V10.LocalGrid exposing (..)

import Evergreen.V10.Bounds
import Dict
import EverySet
import Evergreen.V10.Grid
import Evergreen.V10.Helper
import Evergreen.V10.Units
import Evergreen.V10.User


type alias LocalGrid_ = 
    { grid : Evergreen.V10.Grid.Grid
    , undoHistory : (List (Dict.Dict Evergreen.V10.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V10.Helper.RawCellCoord Int))
    , user : (Evergreen.V10.User.UserId, Evergreen.V10.User.UserData)
    , otherUsers : (List (Evergreen.V10.User.UserId, Evergreen.V10.User.UserData))
    , hiddenUsers : (EverySet.EverySet Evergreen.V10.User.UserId)
    , adminHiddenUsers : (EverySet.EverySet Evergreen.V10.User.UserId)
    , viewBounds : (Evergreen.V10.Bounds.Bounds Evergreen.V10.Units.CellUnit)
    , undoCurrent : (Dict.Dict Evergreen.V10.Helper.RawCellCoord Int)
    }


type LocalGrid
    = LocalGrid LocalGrid_