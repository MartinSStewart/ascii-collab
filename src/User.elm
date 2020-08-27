module User exposing (RawUserId, RenameError(..), UserData(..), UserId, color, isOnline, name, newUser, rawId, rename, userId, withIsOnline)

import ColorIndex exposing (ColorIndex(..))
import List.Extra as List


type UserData
    = User { name : String, color : ColorIndex, isOnline : Bool }


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
        { name = "User " ++ String.fromInt index
        , color = List.getAt (modBy (List.length ColorIndex.colors) index) ColorIndex.colors |> Maybe.withDefault Green
        , isOnline = True
        }
    )


isOnline : UserData -> Bool
isOnline (User user) =
    user.isOnline


rawId : UserId -> Int
rawId (UserId userId_) =
    userId_


name : UserData -> String
name (User user) =
    user.name


rename : List UserData -> String -> UserData -> Result RenameError UserData
rename otherUsers name_ (User user) =
    let
        santizedName =
            String.trim name_
    in
    if String.length santizedName < 2 || String.length santizedName > 12 then
        Err InvalidName

    else if List.any (\otherUserData -> name otherUserData == name_) otherUsers then
        Err NameAlreadyInUse

    else
        User { user | name = santizedName } |> Ok


type RenameError
    = NameAlreadyInUse
    | InvalidName


withIsOnline : Bool -> UserData -> UserData
withIsOnline isOnline_ (User user) =
    User { user | isOnline = isOnline_ }


color : UserData -> ColorIndex
color (User user) =
    user.color
