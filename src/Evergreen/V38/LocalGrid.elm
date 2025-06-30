module Evergreen.V38.LocalGrid exposing (..)

import Dict
import Evergreen.V38.Bounds
import Evergreen.V38.Grid
import Evergreen.V38.Helper
import Evergreen.V38.Units
import Evergreen.V38.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V38.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V38.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V38.Helper.RawCellCoord Int)
    , user : Evergreen.V38.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V38.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V38.User.UserId
    , viewBounds : Evergreen.V38.Bounds.Bounds Evergreen.V38.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V38.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
