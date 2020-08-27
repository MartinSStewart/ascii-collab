module User exposing (RawUserId, UserData(..), UserId, color, isOnline, newUser, rawId, userId, withIsOnline)

import ColorIndex exposing (ColorIndex(..))
import List.Extra as List


type UserData
    = User { color : ColorIndex, isOnline : Bool }


type UserId
    = UserId Int


type alias RawUserId =
    Int


userId : Int -> UserId
userId index =
    UserId index


newUser : Int -> ( UserId, UserData )
newUser index =
    ( userId index
    , User
        { color = List.getAt (modBy (List.length ColorIndex.colors) index) ColorIndex.colors |> Maybe.withDefault Green
        , isOnline = True
        }
    )


isOnline : UserData -> Bool
isOnline (User user) =
    user.isOnline


rawId : UserId -> Int
rawId (UserId userId_) =
    userId_


withIsOnline : Bool -> UserData -> UserData
withIsOnline isOnline_ (User user) =
    User { user | isOnline = isOnline_ }


color : UserData -> ColorIndex
color (User user) =
    user.color
