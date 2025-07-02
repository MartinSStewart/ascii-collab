module Evergreen.V80.GridCell exposing (..)

import Dict
import Evergreen.V80.Ascii
import Evergreen.V80.User
import List.Nonempty


type Cell
    = Cell
        { history :
            List
                { userId : Evergreen.V80.User.UserId
                , position : Int
                , line : List.Nonempty.Nonempty Evergreen.V80.Ascii.Ascii
                }
        , undoPoint : Dict.Dict Evergreen.V80.User.RawUserId Int
        }
