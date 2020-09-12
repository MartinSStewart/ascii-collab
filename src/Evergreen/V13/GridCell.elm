module Evergreen.V13.GridCell exposing (..)

import Evergreen.V13.Ascii
import Dict
import List.Nonempty
import Evergreen.V13.User


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V13.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V13.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V13.User.RawUserId Int)
    }