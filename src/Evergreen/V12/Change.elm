module Evergreen.V12.Change exposing (..)

import Evergreen.V12.Bounds
import Dict
import Evergreen.V12.Grid
import Evergreen.V12.GridCell
import Evergreen.V12.Helper
import Evergreen.V12.Units
import Evergreen.V12.User


type LocalChange
    = LocalGridChange Evergreen.V12.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalToggleUserVisibility Evergreen.V12.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V12.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V12.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V12.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V12.Helper.RawCellCoord Int)
    }
    | ServerUserNew (Evergreen.V12.User.UserId, Evergreen.V12.User.UserData)
    | ServerToggleUserVisibilityForAll Evergreen.V12.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V12.Bounds.Bounds Evergreen.V12.Units.CellUnit) (List ((Evergreen.V12.Helper.Coord Evergreen.V12.Units.CellUnit), Evergreen.V12.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange