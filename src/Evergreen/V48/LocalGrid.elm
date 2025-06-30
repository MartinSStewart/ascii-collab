module Evergreen.V48.LocalGrid exposing (..)

import Dict
import Evergreen.V48.Bounds
import Evergreen.V48.Grid
import Evergreen.V48.Helper
import Evergreen.V48.Units
import Evergreen.V48.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V48.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V48.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V48.Helper.RawCellCoord Int)
    , user : Evergreen.V48.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V48.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V48.User.UserId
    , viewBounds : Evergreen.V48.Bounds.Bounds Evergreen.V48.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V48.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
