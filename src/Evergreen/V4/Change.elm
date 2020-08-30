module Evergreen.V4.Change exposing (..)

import Dict
import Evergreen.V4.Grid
import Evergreen.V4.User


type LocalChange
    = LocalGridChange Evergreen.V4.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalToggleUserVisibility Evergreen.V4.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V4.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V4.User.UserId
    , undoPoints : (Dict.Dict (Int, Int) Int)
    }
    | ServerUserNew (Evergreen.V4.User.UserId, Evergreen.V4.User.UserData)


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange