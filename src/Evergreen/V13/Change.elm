module Evergreen.V13.Change exposing (..)

import Evergreen.V13.Bounds
import Dict
import Evergreen.V13.Grid
import Evergreen.V13.GridCell
import Evergreen.V13.Helper
import Evergreen.V13.Units
import Evergreen.V13.User


type LocalChange
    = LocalGridChange Evergreen.V13.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V13.User.UserId (Evergreen.V13.Helper.Coord Evergreen.V13.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V13.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V13.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V13.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V13.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V13.Helper.RawCellCoord Int)
    }
    | ServerUserNew (Evergreen.V13.User.UserId, Evergreen.V13.User.UserData)
    | ServerToggleUserVisibilityForAll Evergreen.V13.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V13.Bounds.Bounds Evergreen.V13.Units.CellUnit) (List ((Evergreen.V13.Helper.Coord Evergreen.V13.Units.CellUnit), Evergreen.V13.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange