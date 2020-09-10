module Evergreen.V8.Change exposing (..)

import Evergreen.V8.Bounds
import Dict
import Evergreen.V8.Grid
import Evergreen.V8.GridCell
import Evergreen.V8.Helper
import Evergreen.V8.Units
import Evergreen.V8.User


type LocalChange
    = LocalGridChange Evergreen.V8.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalToggleUserVisibility Evergreen.V8.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V8.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V8.User.UserId
    , undoPoints : (Dict.Dict Evergreen.V8.Helper.RawCellCoord Int)
    }
    | ServerUserNew (Evergreen.V8.User.UserId, Evergreen.V8.User.UserData)


type ClientChange
    = ViewBoundsChange (Evergreen.V8.Bounds.Bounds Evergreen.V8.Units.CellUnit) (List ((Evergreen.V8.Helper.Coord Evergreen.V8.Units.CellUnit), Evergreen.V8.GridCell.Cell))


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange