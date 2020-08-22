module Evergreen.V1.User exposing (..)

import Evergreen.V1.ColorIndex


type UserId
    = UserId Int


type alias RawUserId = Int


type User
    = User 
    { name : String
    , id : UserId
    , color : Evergreen.V1.ColorIndex.ColorIndex
    }