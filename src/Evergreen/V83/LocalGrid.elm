module Evergreen.V83.LocalGrid exposing (..)

import Dict
import Evergreen.V83.Bounds
import Evergreen.V83.Grid
import Evergreen.V83.Helper
import Evergreen.V83.Units
import Evergreen.V83.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V83.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V83.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V83.Helper.RawCellCoord Int)
    , user : Evergreen.V83.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V83.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V83.User.UserId
    , viewBounds : Evergreen.V83.Bounds.Bounds Evergreen.V83.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V83.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
