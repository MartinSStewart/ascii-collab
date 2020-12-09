module Evergreen.V45.Change exposing (..)

import Dict
import Evergreen.V45.Bounds
import Evergreen.V45.Grid
import Evergreen.V45.GridCell
import Evergreen.V45.Helper
import Evergreen.V45.Units
import Evergreen.V45.User


type LocalChange
    = LocalGridChange Evergreen.V45.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V45.User.UserId (Evergreen.V45.Helper.Coord Evergreen.V45.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V45.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V45.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V45.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V45.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V45.Helper.RawCellCoord Int)
    }
    | ServerToggleUserVisibilityForAll Evergreen.V45.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V45.Bounds.Bounds Evergreen.V45.Units.CellUnit) (List ((Evergreen.V45.Helper.Coord Evergreen.V45.Units.CellUnit), Evergreen.V45.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange