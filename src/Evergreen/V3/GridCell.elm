module Evergreen.V3.GridCell exposing (..)

import Evergreen.V3.Ascii
import Dict
import List.Nonempty
import Evergreen.V3.User


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V3.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V3.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V3.User.RawUserId Int)
    }