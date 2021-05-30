module Evergreen.V68.Grid exposing (..)

import Dict
import Evergreen.V68.Ascii
import Evergreen.V68.GridCell
import Evergreen.V68.Helper
import Evergreen.V68.Units
import Evergreen.V68.User
import List.Nonempty
import Math.Vector2


type alias LocalGridChange =
    { cellPosition : Evergreen.V68.Helper.Coord Evergreen.V68.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V68.Ascii.Ascii
    }


type alias GridChange =
    { cellPosition : Evergreen.V68.Helper.Coord Evergreen.V68.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V68.Ascii.Ascii
    , userId : Evergreen.V68.User.UserId
    }


type Grid
    = Grid (Dict.Dict ( Int, Int ) Evergreen.V68.GridCell.Cell)


type alias Vertex =
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }
