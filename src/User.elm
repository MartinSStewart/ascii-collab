module User exposing (User(..), UserId, color, fromIndex, id, name, rawId, withName)

import ColorIndex exposing (ColorIndex(..))
import List.Extra as List


type User
    = User { name : String, id : UserId, color : ColorIndex }


type UserId
    = UserId Int


userId : Int -> UserId
userId index =
    UserId index


fromIndex : Int -> User
fromIndex index =
    User
        { name = "User " ++ String.fromInt index
        , id = userId index
        , color = List.getAt (modBy (List.length ColorIndex.colors) index) ColorIndex.colors |> Maybe.withDefault Green
        }


id : User -> UserId
id (User user) =
    user.id


rawId : UserId -> Int
rawId (UserId userId_) =
    userId_


name : User -> String
name (User user) =
    user.name


withName : String -> User -> Maybe User
withName name_ (User user) =
    let
        santizedName =
            String.trim name_
    in
    if String.length santizedName < 1 || String.length santizedName > 12 then
        Nothing

    else
        User { user | name = santizedName } |> Just


color : User -> ColorIndex
color (User user) =
    user.color
