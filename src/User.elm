module User exposing (RawUserId, UserId(..), rawId, userId)


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
