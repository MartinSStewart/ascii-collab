module Evergreen.V4.GridCell exposing (..)

import Evergreen.V4.Ascii
import Dict
import List.Nonempty
import Evergreen.V4.User


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V4.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V4.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V4.User.RawUserId Int)
    }