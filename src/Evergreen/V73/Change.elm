module Evergreen.V73.Change exposing (..)

import Dict
import Evergreen.V73.Bounds
import Evergreen.V73.Grid
import Evergreen.V73.GridCell
import Evergreen.V73.Helper
import Evergreen.V73.Units
import Evergreen.V73.User


type LocalChange
    = LocalGridChange Evergreen.V73.Grid.LocalGridChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V73.User.UserId (Evergreen.V73.Helper.Coord Evergreen.V73.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V73.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V73.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V73.Grid.GridChange
    | ServerUndoPoint
        { userId : Evergreen.V73.User.UserId
        , undoPoints : Dict.Dict Evergreen.V73.Helper.RawCellCoord Int
        }
    | ServerToggleUserVisibilityForAll Evergreen.V73.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V73.Bounds.Bounds Evergreen.V73.Units.CellUnit) (List ( Evergreen.V73.Helper.Coord Evergreen.V73.Units.CellUnit, Evergreen.V73.GridCell.Cell ))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange
