module Evergreen.V45.GridCell exposing (..)

import Dict
import Evergreen.V45.Ascii
import Evergreen.V45.User
import List.Nonempty


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V45.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V45.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V45.User.RawUserId Int)
    }