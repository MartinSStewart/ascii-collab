module User exposing (User(..), UserId, fromSessionId)

import Lamdera exposing (SessionId)


type User
    = User { name : String, id : UserId }


type UserId
    = TempUserId SessionId


fromSessionId : SessionId -> UserId
fromSessionId =
    TempUserId
