module Evergreen.V81.GridCell exposing (..)

import Dict
import Evergreen.V81.Ascii
import Evergreen.V81.User
import List.Nonempty


type Cell
    = Cell
        { history :
            List
                { userId : Evergreen.V81.User.UserId
                , position : Int
                , line : List.Nonempty.Nonempty Evergreen.V81.Ascii.Ascii
                }
        , undoPoint : Dict.Dict Evergreen.V81.User.RawUserId Int
        }
