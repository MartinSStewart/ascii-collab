module Evergreen.V68.GridCell exposing (..)

import Dict
import Evergreen.V68.Ascii
import Evergreen.V68.User
import List.Nonempty


type Cell
    = Cell
        { history :
            List
                { userId : Evergreen.V68.User.UserId
                , position : Int
                , line : List.Nonempty.Nonempty Evergreen.V68.Ascii.Ascii
                }
        , undoPoint : Dict.Dict Evergreen.V68.User.RawUserId Int
        }
