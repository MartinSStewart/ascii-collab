module Evergreen.V73.Grid exposing (..)

import Dict
import Evergreen.V73.Ascii
import Evergreen.V73.GridCell
import Evergreen.V73.Helper
import Evergreen.V73.Units
import Evergreen.V73.User
import List.Nonempty
import Math.Vector2


type alias LocalGridChange =
    { cellPosition : Evergreen.V73.Helper.Coord Evergreen.V73.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V73.Ascii.Ascii
    }


type alias GridChange =
    { cellPosition : Evergreen.V73.Helper.Coord Evergreen.V73.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V73.Ascii.Ascii
    , userId : Evergreen.V73.User.UserId
    }


type Grid
    = Grid (Dict.Dict ( Int, Int ) Evergreen.V73.GridCell.Cell)


type alias Vertex =
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }
