module Evergreen.V47.Change exposing (..)

import Dict
import Evergreen.V47.Bounds
import Evergreen.V47.Grid
import Evergreen.V47.GridCell
import Evergreen.V47.Helper
import Evergreen.V47.Units
import Evergreen.V47.User


type LocalChange
    = LocalGridChange Evergreen.V47.Grid.LocalGridChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V47.User.UserId (Evergreen.V47.Helper.Coord Evergreen.V47.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V47.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V47.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V47.Grid.GridChange
    | ServerUndoPoint 
    { userId : Evergreen.V47.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V47.Helper.RawCellCoord Int)
    }
    | ServerToggleUserVisibilityForAll Evergreen.V47.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V47.Bounds.Bounds Evergreen.V47.Units.CellUnit) (List ((Evergreen.V47.Helper.Coord Evergreen.V47.Units.CellUnit), Evergreen.V47.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange