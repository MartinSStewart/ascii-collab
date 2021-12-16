module Evergreen.V77.Grid exposing (..)

import Dict
import Evergreen.V77.Ascii
import Evergreen.V77.GridCell
import Evergreen.V77.Helper
import Evergreen.V77.Units
import Evergreen.V77.User
import List.Nonempty
import Math.Vector2


type alias LocalGridChange =
    { cellPosition : Evergreen.V77.Helper.Coord Evergreen.V77.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V77.Ascii.Ascii
    }


type alias GridChange =
    { cellPosition : Evergreen.V77.Helper.Coord Evergreen.V77.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V77.Ascii.Ascii
    , userId : Evergreen.V77.User.UserId
    }


type Grid
    = Grid (Dict.Dict ( Int, Int ) Evergreen.V77.GridCell.Cell)


type alias Vertex =
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }
