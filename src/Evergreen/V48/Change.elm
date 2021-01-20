module Evergreen.V48.Change exposing (..)

import Dict
import Evergreen.V48.Bounds
import Evergreen.V48.Grid
import Evergreen.V48.GridCell
import Evergreen.V48.Helper
import Evergreen.V48.Units
import Evergreen.V48.User


type LocalChange
    = LocalGridChange Evergreen.V48.Grid.LocalGridChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V48.User.UserId (Evergreen.V48.Helper.Coord Evergreen.V48.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V48.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V48.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V48.Grid.GridChange
    | ServerUndoPoint 
    { userId : Evergreen.V48.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V48.Helper.RawCellCoord Int)
    }
    | ServerToggleUserVisibilityForAll Evergreen.V48.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V48.Bounds.Bounds Evergreen.V48.Units.CellUnit) (List ((Evergreen.V48.Helper.Coord Evergreen.V48.Units.CellUnit), Evergreen.V48.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange