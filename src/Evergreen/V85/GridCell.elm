module Evergreen.V85.GridCell exposing (..)

import Dict
import Evergreen.V85.Ascii
import Evergreen.V85.User
import List.Nonempty


type Cell
    = Cell
        { history :
            List
                { userId : Evergreen.V85.User.UserId
                , position : Int
                , line : List.Nonempty.Nonempty Evergreen.V85.Ascii.Ascii
                }
        , undoPoint : Dict.Dict Evergreen.V85.User.RawUserId Int
        }
