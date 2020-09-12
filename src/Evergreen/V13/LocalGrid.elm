module Evergreen.V13.LocalGrid exposing (..)

import Evergreen.V13.Bounds
import Dict
import EverySet
import Evergreen.V13.Grid
import Evergreen.V13.Helper
import Evergreen.V13.Units
import Evergreen.V13.User


type alias LocalGrid_ = 
    { grid : Evergreen.V13.Grid.Grid
    , undoHistory : (List (Dict.Dict Evergreen.V13.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V13.Helper.RawCellCoord Int))
    , user : (Evergreen.V13.User.UserId, Evergreen.V13.User.UserData)
    , otherUsers : (List (Evergreen.V13.User.UserId, Evergreen.V13.User.UserData))
    , hiddenUsers : (EverySet.EverySet Evergreen.V13.User.UserId)
    , adminHiddenUsers : (EverySet.EverySet Evergreen.V13.User.UserId)
    , viewBounds : (Evergreen.V13.Bounds.Bounds Evergreen.V13.Units.CellUnit)
    , undoCurrent : (Dict.Dict Evergreen.V13.Helper.RawCellCoord Int)
    }


type LocalGrid
    = LocalGrid LocalGrid_