module Evergreen.V68.Change exposing (..)

import Dict
import Evergreen.V68.Bounds
import Evergreen.V68.Grid
import Evergreen.V68.GridCell
import Evergreen.V68.Helper
import Evergreen.V68.Units
import Evergreen.V68.User


type LocalChange
    = LocalGridChange Evergreen.V68.Grid.LocalGridChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V68.User.UserId (Evergreen.V68.Helper.Coord Evergreen.V68.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V68.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V68.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V68.Grid.GridChange
    | ServerUndoPoint
        { userId : Evergreen.V68.User.UserId
        , undoPoints : Dict.Dict Evergreen.V68.Helper.RawCellCoord Int
        }
    | ServerToggleUserVisibilityForAll Evergreen.V68.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V68.Bounds.Bounds Evergreen.V68.Units.CellUnit) (List ( Evergreen.V68.Helper.Coord Evergreen.V68.Units.CellUnit, Evergreen.V68.GridCell.Cell ))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange
