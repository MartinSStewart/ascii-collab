module Evergreen.V78.GridCell exposing (..)

import Dict
import Evergreen.V78.Ascii
import Evergreen.V78.User
import List.Nonempty


type Cell
    = Cell
        { history :
            List
                { userId : Evergreen.V78.User.UserId
                , position : Int
                , line : List.Nonempty.Nonempty Evergreen.V78.Ascii.Ascii
                }
        , undoPoint : Dict.Dict Evergreen.V78.User.RawUserId Int
        }
