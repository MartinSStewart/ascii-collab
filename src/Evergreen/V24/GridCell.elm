module Evergreen.V24.GridCell exposing (..)

import Evergreen.V24.Ascii
import Dict
import List.Nonempty
import Evergreen.V24.User


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V24.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V24.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V24.User.RawUserId Int)
    }