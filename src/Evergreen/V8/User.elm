module Evergreen.V8.User exposing (..)

import Evergreen.V8.ColorIndex


type UserId
    = UserId Int


type UserData
    = User 
    { color : Evergreen.V8.ColorIndex.ColorIndex
    }


type alias RawUserId = Int