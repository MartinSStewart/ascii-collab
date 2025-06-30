module Evergreen.V45.LocalGrid exposing (..)

import Dict
import Evergreen.V45.Bounds
import Evergreen.V45.Grid
import Evergreen.V45.Helper
import Evergreen.V45.Units
import Evergreen.V45.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V45.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V45.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V45.Helper.RawCellCoord Int)
    , user : Evergreen.V45.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V45.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V45.User.UserId
    , viewBounds : Evergreen.V45.Bounds.Bounds Evergreen.V45.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V45.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
