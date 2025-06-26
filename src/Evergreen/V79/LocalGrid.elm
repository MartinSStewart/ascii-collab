module Evergreen.V79.LocalGrid exposing (..)

import Dict
import Evergreen.V79.Bounds
import Evergreen.V79.Grid
import Evergreen.V79.Helper
import Evergreen.V79.Units
import Evergreen.V79.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V79.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V79.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V79.Helper.RawCellCoord Int)
    , user : Evergreen.V79.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V79.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V79.User.UserId
    , viewBounds : Evergreen.V79.Bounds.Bounds Evergreen.V79.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V79.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
