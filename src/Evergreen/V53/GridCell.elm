module Evergreen.V53.GridCell exposing (..)

import Dict
import Evergreen.V53.Ascii
import Evergreen.V53.User
import List.Nonempty


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V53.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V53.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V53.User.RawUserId Int)
    }