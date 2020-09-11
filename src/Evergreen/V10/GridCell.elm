module Evergreen.V10.GridCell exposing (..)

import Evergreen.V10.Ascii
import Dict
import List.Nonempty
import Evergreen.V10.User


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V10.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V10.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V10.User.RawUserId Int)
    }