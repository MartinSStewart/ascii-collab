module Evergreen.V85.LocalGrid exposing (..)

import Dict
import Evergreen.V85.Bounds
import Evergreen.V85.Grid
import Evergreen.V85.Helper
import Evergreen.V85.Units
import Evergreen.V85.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V85.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V85.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V85.Helper.RawCellCoord Int)
    , user : Evergreen.V85.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V85.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V85.User.UserId
    , viewBounds : Evergreen.V85.Bounds.Bounds Evergreen.V85.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V85.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
