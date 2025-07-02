module Evergreen.V83.GridCell exposing (..)

import Dict
import Evergreen.V83.Ascii
import Evergreen.V83.User
import List.Nonempty


type Cell
    = Cell
        { history :
            List
                { userId : Evergreen.V83.User.UserId
                , position : Int
                , line : List.Nonempty.Nonempty Evergreen.V83.Ascii.Ascii
                }
        , undoPoint : Dict.Dict Evergreen.V83.User.RawUserId Int
        }
