module Evergreen.V3.User exposing (..)

import Evergreen.V3.ColorIndex


type UserId
    = UserId Int


type alias RawUserId = Int


type User
    = User 
    { name : String
    , id : UserId
    , color : Evergreen.V3.ColorIndex.ColorIndex
    }