module User exposing (User(..), UserId, userId)


type User
    = User { name : String, id : UserId }


type UserId
    = TempUserId Int


userId : Int -> UserId
userId index =
    TempUserId index
