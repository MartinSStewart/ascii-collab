module Evergreen.V83.Change exposing (..)

import Dict
import Evergreen.V83.Bounds
import Evergreen.V83.Grid
import Evergreen.V83.GridCell
import Evergreen.V83.Helper
import Evergreen.V83.Units
import Evergreen.V83.User


type LocalChange
    = LocalGridChange Evergreen.V83.Grid.LocalGridChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V83.User.UserId (Evergreen.V83.Helper.Coord Evergreen.V83.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V83.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V83.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V83.Grid.GridChange
    | ServerUndoPoint
        { userId : Evergreen.V83.User.UserId
        , undoPoints : Dict.Dict Evergreen.V83.Helper.RawCellCoord Int
        }
    | ServerToggleUserVisibilityForAll Evergreen.V83.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V83.Bounds.Bounds Evergreen.V83.Units.CellUnit) (List ( Evergreen.V83.Helper.Coord Evergreen.V83.Units.CellUnit, Evergreen.V83.GridCell.Cell ))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange
