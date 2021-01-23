module Evergreen.V53.Change exposing (..)

import Dict
import Evergreen.V53.Bounds
import Evergreen.V53.Grid
import Evergreen.V53.GridCell
import Evergreen.V53.Helper
import Evergreen.V53.Units
import Evergreen.V53.User


type LocalChange
    = LocalGridChange Evergreen.V53.Grid.LocalGridChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V53.User.UserId (Evergreen.V53.Helper.Coord Evergreen.V53.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V53.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V53.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V53.Grid.GridChange
    | ServerUndoPoint 
    { userId : Evergreen.V53.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V53.Helper.RawCellCoord Int)
    }
    | ServerToggleUserVisibilityForAll Evergreen.V53.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V53.Bounds.Bounds Evergreen.V53.Units.CellUnit) (List ((Evergreen.V53.Helper.Coord Evergreen.V53.Units.CellUnit), Evergreen.V53.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange