module User exposing (User(..), UserId, color, fromIndex, id, name)

import ColorIndex exposing (ColorIndex(..))


type User
    = User { name : String, id : UserId, color : ColorIndex }


type UserId
    = TempUserId Int


userId : Int -> UserId
userId index =
    TempUserId index


fromIndex : Int -> User
fromIndex index =
    User { name = "User" ++ String.fromInt index, id = userId index, color = Green }


id : User -> UserId
id (User user) =
    user.id


name : User -> UserId
name (User user) =
    user.name


color : User -> UserId
color (User user) =
    user.color
