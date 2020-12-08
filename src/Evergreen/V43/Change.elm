module Evergreen.V43.Change exposing (..)

import Dict
import Evergreen.V43.Bounds
import Evergreen.V43.Grid
import Evergreen.V43.GridCell
import Evergreen.V43.Helper
import Evergreen.V43.Units
import Evergreen.V43.User


type LocalChange
    = LocalGridChange Evergreen.V43.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V43.User.UserId (Evergreen.V43.Helper.Coord Evergreen.V43.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V43.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V43.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V43.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V43.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V43.Helper.RawCellCoord Int)
    }
    | ServerToggleUserVisibilityForAll Evergreen.V43.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V43.Bounds.Bounds Evergreen.V43.Units.CellUnit) (List ((Evergreen.V43.Helper.Coord Evergreen.V43.Units.CellUnit), Evergreen.V43.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange