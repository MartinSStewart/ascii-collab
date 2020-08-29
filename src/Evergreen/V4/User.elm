module Evergreen.V4.User exposing (..)

import Evergreen.V4.ColorIndex


type UserId
    = UserId Int


type UserData
    = User 
    { color : Evergreen.V4.ColorIndex.ColorIndex
    }


type alias RawUserId = Int