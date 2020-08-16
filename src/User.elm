module User exposing (User(..), UserId, color, fromIndex, id, name, withName)

import ColorIndex exposing (ColorIndex(..))
import List.Extra as List


type User
    = User { name : String, id : UserId, color : ColorIndex }


type UserId
    = TempUserId Int


userId : Int -> UserId
userId index =
    TempUserId index


fromIndex : Int -> User
fromIndex index =
    User
        { name = "User" ++ String.fromInt index
        , id = userId index
        , color = List.getAt (modBy (List.length ColorIndex.colors) index) ColorIndex.colors |> Maybe.withDefault Green
        }


id : User -> UserId
id (User user) =
    user.id


name : User -> String
name (User user) =
    user.name


withName : String -> User -> Maybe User
withName name_ (User user) =
    let
        santizedName =
            String.trim name_
    in
    if String.length santizedName < 1 || String.length santizedName > 20 then
        Nothing

    else
        User { user | name = santizedName } |> Just


color : User -> ColorIndex
color (User user) =
    user.color
