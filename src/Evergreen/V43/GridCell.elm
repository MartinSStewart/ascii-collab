module Evergreen.V43.GridCell exposing (..)

import Dict
import Evergreen.V43.Ascii
import Evergreen.V43.User
import List.Nonempty


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V43.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V43.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V43.User.RawUserId Int)
    }