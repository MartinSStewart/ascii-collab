module Evergreen.V8.GridCell exposing (..)

import Evergreen.V8.Ascii
import Dict
import List.Nonempty
import Evergreen.V8.User


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V8.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V8.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V8.User.RawUserId Int)
    }