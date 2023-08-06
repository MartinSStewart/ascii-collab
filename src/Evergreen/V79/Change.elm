module Evergreen.V79.Change exposing (..)

import Dict
import Evergreen.V79.Bounds
import Evergreen.V79.Grid
import Evergreen.V79.GridCell
import Evergreen.V79.Helper
import Evergreen.V79.Units
import Evergreen.V79.User


type LocalChange
    = LocalGridChange Evergreen.V79.Grid.LocalGridChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V79.User.UserId (Evergreen.V79.Helper.Coord Evergreen.V79.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V79.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V79.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V79.Grid.GridChange
    | ServerUndoPoint
        { userId : Evergreen.V79.User.UserId
        , undoPoints : Dict.Dict Evergreen.V79.Helper.RawCellCoord Int
        }
    | ServerToggleUserVisibilityForAll Evergreen.V79.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V79.Bounds.Bounds Evergreen.V79.Units.CellUnit) (List ( Evergreen.V79.Helper.Coord Evergreen.V79.Units.CellUnit, Evergreen.V79.GridCell.Cell ))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange
