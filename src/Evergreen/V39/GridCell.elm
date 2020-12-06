module Evergreen.V39.GridCell exposing (..)

import Dict
import Evergreen.V39.Ascii
import Evergreen.V39.User
import List.Nonempty


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V39.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V39.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V39.User.RawUserId Int)
    }