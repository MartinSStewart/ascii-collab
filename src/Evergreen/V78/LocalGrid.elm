module Evergreen.V78.LocalGrid exposing (..)

import Dict
import Evergreen.V78.Bounds
import Evergreen.V78.Grid
import Evergreen.V78.Helper
import Evergreen.V78.Units
import Evergreen.V78.User
import EverySet


type alias LocalGrid_ =
    { grid : Evergreen.V78.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V78.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V78.Helper.RawCellCoord Int)
    , user : Evergreen.V78.User.UserId
    , hiddenUsers : EverySet.EverySet Evergreen.V78.User.UserId
    , adminHiddenUsers : EverySet.EverySet Evergreen.V78.User.UserId
    , viewBounds : Evergreen.V78.Bounds.Bounds Evergreen.V78.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V78.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
