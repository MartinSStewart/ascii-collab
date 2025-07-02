module Evergreen.V81.LocalGrid exposing (..)

import Dict
import Evergreen.V81.Bounds
import Evergreen.V81.Grid
import Evergreen.V81.Helper
import Evergreen.V81.Units
import Evergreen.V81.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V81.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V81.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V81.Helper.RawCellCoord Int)
    , user : Evergreen.V81.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V81.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V81.User.UserId
    , viewBounds : Evergreen.V81.Bounds.Bounds Evergreen.V81.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V81.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
