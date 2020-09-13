module Evergreen.V14.GridCell exposing (..)

import Evergreen.V14.Ascii
import Dict
import List.Nonempty
import Evergreen.V14.User


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V14.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V14.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V14.User.RawUserId Int)
    }