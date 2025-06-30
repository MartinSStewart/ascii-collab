module Evergreen.V24.LocalGrid exposing (..)

import Dict
import Evergreen.V24.Bounds
import Evergreen.V24.Grid
import Evergreen.V24.Helper
import Evergreen.V24.Units
import Evergreen.V24.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V24.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V24.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V24.Helper.RawCellCoord Int)
    , user : Evergreen.V24.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V24.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V24.User.UserId
    , viewBounds : Evergreen.V24.Bounds.Bounds Evergreen.V24.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V24.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
