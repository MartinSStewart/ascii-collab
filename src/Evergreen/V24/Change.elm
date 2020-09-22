module Evergreen.V24.Change exposing (..)

import Evergreen.V24.Bounds
import Dict
import Evergreen.V24.Grid
import Evergreen.V24.GridCell
import Evergreen.V24.Helper
import Evergreen.V24.Units
import Evergreen.V24.User


type LocalChange
    = LocalGridChange Evergreen.V24.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V24.User.UserId (Evergreen.V24.Helper.Coord Evergreen.V24.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V24.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V24.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V24.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V24.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V24.Helper.RawCellCoord Int)
    }
    | ServerToggleUserVisibilityForAll Evergreen.V24.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V24.Bounds.Bounds Evergreen.V24.Units.CellUnit) (List ((Evergreen.V24.Helper.Coord Evergreen.V24.Units.CellUnit), Evergreen.V24.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange