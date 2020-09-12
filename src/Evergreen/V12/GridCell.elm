module Evergreen.V12.GridCell exposing (..)

import Evergreen.V12.Ascii
import Dict
import List.Nonempty
import Evergreen.V12.User


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V12.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V12.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V12.User.RawUserId Int)
    }