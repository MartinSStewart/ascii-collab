module Evergreen.V38.Change exposing (..)

import Dict
import Evergreen.V38.Bounds
import Evergreen.V38.Grid
import Evergreen.V38.GridCell
import Evergreen.V38.Helper
import Evergreen.V38.Units
import Evergreen.V38.User


type LocalChange
    = LocalGridChange Evergreen.V38.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V38.User.UserId (Evergreen.V38.Helper.Coord Evergreen.V38.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V38.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V38.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V38.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V38.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V38.Helper.RawCellCoord Int)
    }
    | ServerToggleUserVisibilityForAll Evergreen.V38.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V38.Bounds.Bounds Evergreen.V38.Units.CellUnit) (List ((Evergreen.V38.Helper.Coord Evergreen.V38.Units.CellUnit), Evergreen.V38.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange