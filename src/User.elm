module User exposing (RawUserId, UserId(..), codec, rawId, userId)

import Serialize


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


codec : Serialize.Codec e UserId
codec =
    Serialize.customType
        (\userIdEncoder value ->
            case value of
                UserId arg0 ->
                    userIdEncoder arg0
        )
        |> Serialize.variant1 UserId Serialize.int
        |> Serialize.finishCustomType
