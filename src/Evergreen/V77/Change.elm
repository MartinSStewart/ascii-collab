module Evergreen.V77.Change exposing (..)

import Dict
import Evergreen.V77.Bounds
import Evergreen.V77.Grid
import Evergreen.V77.GridCell
import Evergreen.V77.Helper
import Evergreen.V77.Units
import Evergreen.V77.User


type LocalChange
    = LocalGridChange Evergreen.V77.Grid.LocalGridChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V77.User.UserId (Evergreen.V77.Helper.Coord Evergreen.V77.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V77.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V77.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V77.Grid.GridChange
    | ServerUndoPoint
        { userId : Evergreen.V77.User.UserId
        , undoPoints : Dict.Dict Evergreen.V77.Helper.RawCellCoord Int
        }
    | ServerToggleUserVisibilityForAll Evergreen.V77.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V77.Bounds.Bounds Evergreen.V77.Units.CellUnit) (List ( Evergreen.V77.Helper.Coord Evergreen.V77.Units.CellUnit, Evergreen.V77.GridCell.Cell ))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange
