module Evergreen.V80.Change exposing (..)

import Dict
import Evergreen.V80.Bounds
import Evergreen.V80.Grid
import Evergreen.V80.GridCell
import Evergreen.V80.Helper
import Evergreen.V80.Units
import Evergreen.V80.User


type LocalChange
    = LocalGridChange Evergreen.V80.Grid.LocalGridChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V80.User.UserId (Evergreen.V80.Helper.Coord Evergreen.V80.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V80.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V80.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V80.Grid.GridChange
    | ServerUndoPoint
        { userId : Evergreen.V80.User.UserId
        , undoPoints : Dict.Dict Evergreen.V80.Helper.RawCellCoord Int
        }
    | ServerToggleUserVisibilityForAll Evergreen.V80.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V80.Bounds.Bounds Evergreen.V80.Units.CellUnit) (List ( Evergreen.V80.Helper.Coord Evergreen.V80.Units.CellUnit, Evergreen.V80.GridCell.Cell ))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange
