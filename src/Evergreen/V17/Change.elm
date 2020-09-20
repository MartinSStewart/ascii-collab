module Evergreen.V17.Change exposing (..)

import Evergreen.V17.Bounds
import Dict
import Evergreen.V17.Grid
import Evergreen.V17.GridCell
import Evergreen.V17.Helper
import Evergreen.V17.Units
import Evergreen.V17.User


type LocalChange
    = LocalGridChange Evergreen.V17.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V17.User.UserId (Evergreen.V17.Helper.Coord Evergreen.V17.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V17.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V17.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V17.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V17.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V17.Helper.RawCellCoord Int)
    }
    | ServerToggleUserVisibilityForAll Evergreen.V17.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V17.Bounds.Bounds Evergreen.V17.Units.CellUnit) (List ((Evergreen.V17.Helper.Coord Evergreen.V17.Units.CellUnit), Evergreen.V17.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange