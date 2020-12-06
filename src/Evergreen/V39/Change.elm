module Evergreen.V39.Change exposing (..)

import Dict
import Evergreen.V39.Bounds
import Evergreen.V39.Grid
import Evergreen.V39.GridCell
import Evergreen.V39.Helper
import Evergreen.V39.Units
import Evergreen.V39.User


type LocalChange
    = LocalGridChange Evergreen.V39.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V39.User.UserId (Evergreen.V39.Helper.Coord Evergreen.V39.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V39.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V39.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V39.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V39.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V39.Helper.RawCellCoord Int)
    }
    | ServerToggleUserVisibilityForAll Evergreen.V39.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V39.Bounds.Bounds Evergreen.V39.Units.CellUnit) (List ((Evergreen.V39.Helper.Coord Evergreen.V39.Units.CellUnit), Evergreen.V39.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange