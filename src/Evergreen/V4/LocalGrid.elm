module Evergreen.V4.LocalGrid exposing (..)

import Dict
import EverySet
import Evergreen.V4.Grid
import Evergreen.V4.User


type alias LocalGrid_ = 
    { grid : Evergreen.V4.Grid.Grid
    , undoHistory : (List (Dict.Dict (Int, Int) Int))
    , redoHistory : (List (Dict.Dict (Int, Int) Int))
    , user : (Evergreen.V4.User.UserId, Evergreen.V4.User.UserData)
    , otherUsers : (List (Evergreen.V4.User.UserId, Evergreen.V4.User.UserData))
    , hiddenUsers : (EverySet.EverySet Evergreen.V4.User.UserId)
    }


type LocalGrid
    = LocalGrid LocalGrid_