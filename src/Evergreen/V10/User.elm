module Evergreen.V10.User exposing (..)

import Evergreen.V10.ColorIndex


type UserId
    = UserId Int


type UserData
    = User 
    { color : Evergreen.V10.ColorIndex.ColorIndex
    }


type alias RawUserId = Int