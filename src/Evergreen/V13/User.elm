module Evergreen.V13.User exposing (..)

import Evergreen.V13.ColorIndex


type UserId
    = UserId Int


type UserData
    = User 
    { color : Evergreen.V13.ColorIndex.ColorIndex
    }


type alias RawUserId = Int