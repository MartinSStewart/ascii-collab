module Evergreen.V43.LocalGrid exposing (..)

import Dict
import Evergreen.V43.Bounds
import Evergreen.V43.Grid
import Evergreen.V43.Helper
import Evergreen.V43.Units
import Evergreen.V43.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V43.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V43.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V43.Helper.RawCellCoord Int)
    , user : Evergreen.V43.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V43.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V43.User.UserId
    , viewBounds : Evergreen.V43.Bounds.Bounds Evergreen.V43.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V43.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
