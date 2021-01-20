module Evergreen.V48.GridCell exposing (..)

import Dict
import Evergreen.V48.Ascii
import Evergreen.V48.User
import List.Nonempty


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V48.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V48.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V48.User.RawUserId Int)
    }