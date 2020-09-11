module Evergreen.V10.Change exposing (..)

import Evergreen.V10.Bounds
import Dict
import Evergreen.V10.Grid
import Evergreen.V10.GridCell
import Evergreen.V10.Helper
import Evergreen.V10.Units
import Evergreen.V10.User


type LocalChange
    = LocalGridChange Evergreen.V10.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalToggleUserVisibility Evergreen.V10.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V10.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V10.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V10.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V10.Helper.RawCellCoord Int)
    }
    | ServerUserNew (Evergreen.V10.User.UserId, Evergreen.V10.User.UserData)
    | ServerToggleUserVisibilityForAll Evergreen.V10.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V10.Bounds.Bounds Evergreen.V10.Units.CellUnit) (List ((Evergreen.V10.Helper.Coord Evergreen.V10.Units.CellUnit), Evergreen.V10.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange