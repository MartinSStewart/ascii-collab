module Evergreen.V50.Change exposing (..)

import Dict
import Evergreen.V50.Bounds
import Evergreen.V50.Grid
import Evergreen.V50.GridCell
import Evergreen.V50.Helper
import Evergreen.V50.Units
import Evergreen.V50.User


type LocalChange
    = LocalGridChange Evergreen.V50.Grid.LocalGridChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V50.User.UserId (Evergreen.V50.Helper.Coord Evergreen.V50.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V50.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V50.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V50.Grid.GridChange
    | ServerUndoPoint 
    { userId : Evergreen.V50.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V50.Helper.RawCellCoord Int)
    }
    | ServerToggleUserVisibilityForAll Evergreen.V50.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V50.Bounds.Bounds Evergreen.V50.Units.CellUnit) (List ((Evergreen.V50.Helper.Coord Evergreen.V50.Units.CellUnit), Evergreen.V50.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange