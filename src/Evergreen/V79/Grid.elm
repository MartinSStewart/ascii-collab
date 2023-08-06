module Evergreen.V79.Grid exposing (..)

import Dict
import Evergreen.V79.Ascii
import Evergreen.V79.GridCell
import Evergreen.V79.Helper
import Evergreen.V79.Units
import Evergreen.V79.User
import List.Nonempty
import Math.Vector2


type alias LocalGridChange =
    { cellPosition : Evergreen.V79.Helper.Coord Evergreen.V79.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V79.Ascii.Ascii
    }


type alias GridChange =
    { cellPosition : Evergreen.V79.Helper.Coord Evergreen.V79.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V79.Ascii.Ascii
    , userId : Evergreen.V79.User.UserId
    }


type Grid
    = Grid (Dict.Dict ( Int, Int ) Evergreen.V79.GridCell.Cell)


type alias Vertex =
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }
