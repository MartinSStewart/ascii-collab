module Evergreen.V47.GridCell exposing (..)

import Dict
import Evergreen.V47.Ascii
import Evergreen.V47.User
import List.Nonempty


type Cell
    = Cell 
    { history : (List 
    { userId : Evergreen.V47.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V47.Ascii.Ascii)
    })
    , undoPoint : (Dict.Dict Evergreen.V47.User.RawUserId Int)
    }