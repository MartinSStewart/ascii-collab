module Evergreen.V3.GridCell exposing (..)

import Evergreen.V3.Ascii
import List.Nonempty
import Evergreen.V3.User


type Cell
    = Cell (List 
    { userId : Evergreen.V3.User.UserId
    , position : Int
    , line : (List.Nonempty.Nonempty Evergreen.V3.Ascii.Ascii)
    })