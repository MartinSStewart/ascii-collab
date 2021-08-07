module Evergreen.V73.GridCell exposing (..)

import Dict
import Evergreen.V73.Ascii
import Evergreen.V73.User
import List.Nonempty


type Cell
    = Cell
        { history :
            List
                { userId : Evergreen.V73.User.UserId
                , position : Int
                , line : List.Nonempty.Nonempty Evergreen.V73.Ascii.Ascii
                }
        , undoPoint : Dict.Dict Evergreen.V73.User.RawUserId Int
        }
