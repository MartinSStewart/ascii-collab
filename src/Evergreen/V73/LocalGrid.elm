module Evergreen.V73.LocalGrid exposing (..)

import Dict
import Evergreen.V73.Bounds
import Evergreen.V73.Grid
import Evergreen.V73.Helper
import Evergreen.V73.Units
import Evergreen.V73.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V73.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V73.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V73.Helper.RawCellCoord Int)
    , user : Evergreen.V73.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V73.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V73.User.UserId
    , viewBounds : Evergreen.V73.Bounds.Bounds Evergreen.V73.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V73.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
