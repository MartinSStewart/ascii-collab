module Evergreen.V28.Change exposing (..)

import Evergreen.V28.Bounds
import Dict
import Evergreen.V28.Grid
import Evergreen.V28.GridCell
import Evergreen.V28.Helper
import Evergreen.V28.Units
import Evergreen.V28.User


type LocalChange
    = LocalGridChange Evergreen.V28.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V28.User.UserId (Evergreen.V28.Helper.Coord Evergreen.V28.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V28.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V28.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V28.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V28.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V28.Helper.RawCellCoord Int)
    }
    | ServerToggleUserVisibilityForAll Evergreen.V28.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V28.Bounds.Bounds Evergreen.V28.Units.CellUnit) (List ((Evergreen.V28.Helper.Coord Evergreen.V28.Units.CellUnit), Evergreen.V28.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange