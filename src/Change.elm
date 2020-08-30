module Change exposing (..)

import Dict exposing (Dict)
import Grid
import User exposing (UserData, UserId)


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange


type LocalChange
    = LocalGridChange Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalToggleUserVisibility UserId


type ServerChange
    = ServerGridChange Grid.Change
    | ServerUndoPoint { userId : UserId, undoPoints : Dict ( Int, Int ) Int }
    | ServerUserNew ( UserId, UserData )
