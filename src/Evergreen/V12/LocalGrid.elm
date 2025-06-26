module Evergreen.V12.LocalGrid exposing (..)

import Dict
import Evergreen.V12.Bounds
import Evergreen.V12.Grid
import Evergreen.V12.Helper
import Evergreen.V12.Units
import Evergreen.V12.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V12.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V12.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V12.Helper.RawCellCoord Int)
    , user : ( Evergreen.V12.User.UserId, Evergreen.V12.User.UserData )
    , otherUsers : List ( Evergreen.V12.User.UserId, Evergreen.V12.User.UserData )
    , hiddenUsers : SeqSet.SeqSet Evergreen.V12.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V12.User.UserId
    , viewBounds : Evergreen.V12.Bounds.Bounds Evergreen.V12.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V12.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
