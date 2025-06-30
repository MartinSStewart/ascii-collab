module Evergreen.V8.LocalGrid exposing (..)

import Dict
import Evergreen.V8.Bounds
import Evergreen.V8.Grid
import Evergreen.V8.Helper
import Evergreen.V8.Units
import Evergreen.V8.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V8.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V8.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V8.Helper.RawCellCoord Int)
    , user : ( Evergreen.V8.User.UserId, Evergreen.V8.User.UserData )
    , otherUsers : List ( Evergreen.V8.User.UserId, Evergreen.V8.User.UserData )
    , hiddenUsers : SeqSet.SeqSet Evergreen.V8.User.UserId
    , viewBounds : Evergreen.V8.Bounds.Bounds Evergreen.V8.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V8.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
