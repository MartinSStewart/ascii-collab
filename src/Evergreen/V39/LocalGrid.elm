module Evergreen.V39.LocalGrid exposing (..)

import Dict
import Evergreen.V39.Bounds
import Evergreen.V39.Grid
import Evergreen.V39.Helper
import Evergreen.V39.Units
import Evergreen.V39.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V39.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V39.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V39.Helper.RawCellCoord Int)
    , user : Evergreen.V39.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V39.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V39.User.UserId
    , viewBounds : Evergreen.V39.Bounds.Bounds Evergreen.V39.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V39.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
