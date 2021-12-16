module Evergreen.V77.GridCell exposing (..)

import Dict
import Evergreen.V77.Ascii
import Evergreen.V77.User
import List.Nonempty


type Cell
    = Cell
        { history :
            List
                { userId : Evergreen.V77.User.UserId
                , position : Int
                , line : List.Nonempty.Nonempty Evergreen.V77.Ascii.Ascii
                }
        , undoPoint : Dict.Dict Evergreen.V77.User.RawUserId Int
        }
