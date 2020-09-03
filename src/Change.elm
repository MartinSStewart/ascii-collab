module Change exposing (Change(..), ClientChange(..), LocalChange(..), ServerChange(..))

import Bounds exposing (Bounds)
import Dict exposing (Dict)
import Grid
import GridCell
import Helper exposing (Coord, RawCellCoord)
import Units exposing (CellUnit)
import User exposing (UserData, UserId)


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange
    | ClientChange ClientChange


type LocalChange
    = LocalGridChange Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalToggleUserVisibility UserId


type ClientChange
    = ViewBoundsChange (Bounds CellUnit) (List ( Coord CellUnit, GridCell.Cell ))


type ServerChange
    = ServerGridChange Grid.Change
    | ServerUndoPoint { userId : UserId, undoPoints : Dict RawCellCoord Int }
    | ServerUserNew ( UserId, UserData )
