module Evergreen.V85.Change exposing (..)

import Dict
import Evergreen.V85.Bounds
import Evergreen.V85.Grid
import Evergreen.V85.GridCell
import Evergreen.V85.Helper
import Evergreen.V85.Units
import Evergreen.V85.User


type LocalChange
    = LocalGridChange Evergreen.V85.Grid.LocalGridChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V85.User.UserId (Evergreen.V85.Helper.Coord Evergreen.V85.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V85.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V85.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V85.Grid.GridChange
    | ServerUndoPoint
        { userId : Evergreen.V85.User.UserId
        , undoPoints : Dict.Dict Evergreen.V85.Helper.RawCellCoord Int
        }
    | ServerToggleUserVisibilityForAll Evergreen.V85.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V85.Bounds.Bounds Evergreen.V85.Units.CellUnit) (List ( Evergreen.V85.Helper.Coord Evergreen.V85.Units.CellUnit, Evergreen.V85.GridCell.Cell ))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange
