module User exposing (RawUserId, UserData(..), UserId(..), color, newUser, rawId, userId)

import ColorIndex exposing (ColorIndex(..))
import List.Extra as List


type UserData
    = User { color : ColorIndex }


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
        }
    )


rawId : UserId -> Int
rawId (UserId userId_) =
    userId_


color : UserData -> ColorIndex
color (User user) =
    user.color
