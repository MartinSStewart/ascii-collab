module Evergreen.V28.LocalGrid exposing (..)

import Dict
import Evergreen.V28.Bounds
import Evergreen.V28.Grid
import Evergreen.V28.Helper
import Evergreen.V28.Units
import Evergreen.V28.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V28.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V28.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V28.Helper.RawCellCoord Int)
    , user : Evergreen.V28.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V28.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V28.User.UserId
    , viewBounds : Evergreen.V28.Bounds.Bounds Evergreen.V28.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V28.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
