module Evergreen.V83.Grid exposing (..)

import Dict
import Evergreen.V83.Ascii
import Evergreen.V83.GridCell
import Evergreen.V83.Helper
import Evergreen.V83.Units
import Evergreen.V83.User
import List.Nonempty
import Math.Vector2


type alias LocalGridChange =
    { cellPosition : Evergreen.V83.Helper.Coord Evergreen.V83.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V83.Ascii.Ascii
    }


type alias GridChange =
    { cellPosition : Evergreen.V83.Helper.Coord Evergreen.V83.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V83.Ascii.Ascii
    , userId : Evergreen.V83.User.UserId
    }


type Grid
    = Grid (Dict.Dict ( Int, Int ) Evergreen.V83.GridCell.Cell)


type alias Vertex =
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }
