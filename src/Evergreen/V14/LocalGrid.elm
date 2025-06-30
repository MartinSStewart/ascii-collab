module Evergreen.V14.LocalGrid exposing (..)

import Dict
import Evergreen.V14.Bounds
import Evergreen.V14.Grid
import Evergreen.V14.Helper
import Evergreen.V14.Units
import Evergreen.V14.User
import SeqSet


type alias LocalGrid_ =
    { grid : Evergreen.V14.Grid.Grid
    , undoHistory : List (Dict.Dict Evergreen.V14.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V14.Helper.RawCellCoord Int)
    , user : ( Evergreen.V14.User.UserId, Evergreen.V14.User.UserData )
    , otherUsers : List ( Evergreen.V14.User.UserId, Evergreen.V14.User.UserData )
    , hiddenUsers : SeqSet.SeqSet Evergreen.V14.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V14.User.UserId
    , viewBounds : Evergreen.V14.Bounds.Bounds Evergreen.V14.Units.CellUnit
    , undoCurrent : Dict.Dict Evergreen.V14.Helper.RawCellCoord Int
    }


type LocalGrid
    = LocalGrid LocalGrid_
