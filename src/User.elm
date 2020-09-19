module User exposing (RawUserId, UserId(..), rawId, userId)

import Angle
import Basics.Extra as Basics
import ColorHelper
import Element
import Random


type UserId
    = UserId Int


type alias RawUserId =
    Int


userId : Int -> UserId
userId index =
    UserId index


rawId : UserId -> Int
rawId (UserId userId_) =
    userId_
