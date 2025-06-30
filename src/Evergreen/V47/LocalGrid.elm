module Evergreen.V47.LocalGrid exposing (..)

import Dict
import Evergreen.V47.Bounds
import Evergreen.V47.Grid
import Evergreen.V47.Helper
import Evergreen.V47.Units
import Evergreen.V47.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V47.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V47.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V47.Helper.RawCellCoord Int)
    , user : Evergreen.V47.User.UserId
    , hiddenUsers : SeqSet.SeqSet Evergreen.V47.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V47.User.UserId
    , viewBounds : Evergreen.V47.Bounds.Bounds Evergreen.V47.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V47.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
