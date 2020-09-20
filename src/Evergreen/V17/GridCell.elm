module Evergreen.V17.GridCell exposing (..)

import Evergreen.V17.Ascii
import Dict
import List.Nonempty
import Evergreen.V17.User


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V17.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V17.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V17.User.RawUserId Int)
    }