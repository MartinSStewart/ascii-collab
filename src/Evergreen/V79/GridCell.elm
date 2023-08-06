module Evergreen.V79.GridCell exposing (..)

import Dict
import Evergreen.V79.Ascii
import Evergreen.V79.User
import List.Nonempty


type Cell
    = Cell
        { history :
            List
                { userId : Evergreen.V79.User.UserId
                , position : Int
                , line : List.Nonempty.Nonempty Evergreen.V79.Ascii.Ascii
                }
        , undoPoint : Dict.Dict Evergreen.V79.User.RawUserId Int
        }
