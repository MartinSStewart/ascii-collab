module Evergreen.V14.User exposing (..)

import Evergreen.V14.ColorIndex


type UserId
    = UserId Int


type UserData
    = User 
    { color : Evergreen.V14.ColorIndex.ColorIndex
    }


type alias RawUserId = Int