module Evergreen.V1.GridCell exposing (..)

import Evergreen.V1.Ascii
import Dict
import List.Nonempty
import Evergreen.V1.User


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V1.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V1.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V1.User.RawUserId Int)
    }