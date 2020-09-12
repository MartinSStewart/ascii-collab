module Evergreen.V12.User exposing (..)

import Evergreen.V12.ColorIndex


type UserId
    = UserId Int


type UserData
    = User 
    { color : Evergreen.V12.ColorIndex.ColorIndex
    }


type alias RawUserId = Int