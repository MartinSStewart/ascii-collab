module Evergreen.V17.LocalGrid exposing (..)

import Dict
import Evergreen.V17.Bounds
import Evergreen.V17.Grid
import Evergreen.V17.Helper
import Evergreen.V17.Units
import Evergreen.V17.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V17.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V17.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V17.Helper.RawCellCoord Int)
    , user : Evergreen.V17.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V17.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V17.User.UserId
    , viewBounds : Evergreen.V17.Bounds.Bounds Evergreen.V17.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V17.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
