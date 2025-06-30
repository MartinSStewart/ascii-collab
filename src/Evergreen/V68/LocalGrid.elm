module Evergreen.V68.LocalGrid exposing (..)

import Dict
import Evergreen.V68.Bounds
import Evergreen.V68.Grid
import Evergreen.V68.Helper
import Evergreen.V68.Units
import Evergreen.V68.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V68.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V68.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V68.Helper.RawCellCoord Int)
    , user : Evergreen.V68.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V68.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V68.User.UserId
    , viewBounds : Evergreen.V68.Bounds.Bounds Evergreen.V68.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V68.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
