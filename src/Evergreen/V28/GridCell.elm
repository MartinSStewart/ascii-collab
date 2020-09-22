module Evergreen.V28.GridCell exposing (..)

import Evergreen.V28.Ascii
import Dict
import List.Nonempty
import Evergreen.V28.User


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V28.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V28.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V28.User.RawUserId Int)
    }