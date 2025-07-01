module Evergreen.V80.Grid exposing (..)

import Dict
import Evergreen.V80.Ascii
import Evergreen.V80.GridCell
import Evergreen.V80.Helper
import Evergreen.V80.Units
import Evergreen.V80.User
import List.Nonempty
import Math.Vector2


type alias LocalGridChange =
    { cellPosition : Evergreen.V80.Helper.Coord Evergreen.V80.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V80.Ascii.Ascii
    }


type alias GridChange =
    { cellPosition : Evergreen.V80.Helper.Coord Evergreen.V80.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V80.Ascii.Ascii
    , userId : Evergreen.V80.User.UserId
    }


type Grid
    = Grid (Dict.Dict ( Int, Int ) Evergreen.V80.GridCell.Cell)


type alias Vertex =
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }
