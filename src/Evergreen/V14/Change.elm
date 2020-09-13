module Evergreen.V14.Change exposing (..)

import Evergreen.V14.Bounds
import Dict
import Evergreen.V14.Grid
import Evergreen.V14.GridCell
import Evergreen.V14.Helper
import Evergreen.V14.Units
import Evergreen.V14.User


type LocalChange
    = LocalGridChange Evergreen.V14.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V14.User.UserId (Evergreen.V14.Helper.Coord Evergreen.V14.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V14.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V14.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V14.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V14.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V14.Helper.RawCellCoord Int)
    }
    | ServerUserNew (Evergreen.V14.User.UserId, Evergreen.V14.User.UserData)
    | ServerToggleUserVisibilityForAll Evergreen.V14.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V14.Bounds.Bounds Evergreen.V14.Units.CellUnit) (List ((Evergreen.V14.Helper.Coord Evergreen.V14.Units.CellUnit), Evergreen.V14.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange