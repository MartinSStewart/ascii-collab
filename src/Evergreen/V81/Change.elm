module Evergreen.V81.Change exposing (..)

import Dict
import Evergreen.V81.Bounds
import Evergreen.V81.Grid
import Evergreen.V81.GridCell
import Evergreen.V81.Helper
import Evergreen.V81.Units
import Evergreen.V81.User


type LocalChange
    = LocalGridChange Evergreen.V81.Grid.LocalGridChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalHideUser Evergreen.V81.User.UserId (Evergreen.V81.Helper.Coord Evergreen.V81.Units.AsciiUnit)
    | LocalUnhideUser Evergreen.V81.User.UserId
    | LocalToggleUserVisibilityForAll Evergreen.V81.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V81.Grid.GridChange
    | ServerUndoPoint
        { userId : Evergreen.V81.User.UserId
        , undoPoints : Dict.Dict Evergreen.V81.Helper.RawCellCoord Int
        }
    | ServerToggleUserVisibilityForAll Evergreen.V81.User.UserId


type ClientChange
    = ViewBoundsChange (Evergreen.V81.Bounds.Bounds Evergreen.V81.Units.CellUnit) (List ( Evergreen.V81.Helper.Coord Evergreen.V81.Units.CellUnit, Evergreen.V81.GridCell.Cell ))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange
