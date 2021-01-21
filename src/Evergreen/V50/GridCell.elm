module Evergreen.V50.GridCell exposing (..)

import Dict
import Evergreen.V50.Ascii
import Evergreen.V50.User
import List.Nonempty


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V50.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V50.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V50.User.RawUserId Int)
    }