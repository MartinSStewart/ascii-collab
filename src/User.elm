module User exposing (User(..), UserId(..))

import Lamdera exposing (SessionId)


type User
    = User { name : String, id : UserId }


type UserId
    = TempUserId SessionId
    | UserId Int
