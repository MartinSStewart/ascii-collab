module Evergreen.V78.Change exposing (..)

import Dict
import Evergreen.V78.Bounds
import Evergreen.V78.Grid
import Evergreen.V78.GridCell
import Evergreen.V78.Helper
import Evergreen.V78.Units
import Evergreen.V78.User


type LocalChange
    = LocalGridChange Evergreen.V78.Grid.LocalGridChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V78.User.UserId (Evergreen.V78.Helper.Coord Evergreen.V78.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V78.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V78.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V78.Grid.GridChange
    | ServerUndoPoint
        { userId : Evergreen.V78.User.UserId
        , undoPoints : Dict.Dict Evergreen.V78.Helper.RawCellCoord Int
        }
    | ServerToggleUserVisibilityForAll Evergreen.V78.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V78.Bounds.Bounds Evergreen.V78.Units.CellUnit) (List ( Evergreen.V78.Helper.Coord Evergreen.V78.Units.CellUnit, Evergreen.V78.GridCell.Cell ))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange
