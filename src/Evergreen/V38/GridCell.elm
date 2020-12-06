module Evergreen.V38.GridCell exposing (..)

import Dict
import Evergreen.V38.Ascii
import Evergreen.V38.User
import List.Nonempty


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V38.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V38.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V38.User.RawUserId Int)
    }