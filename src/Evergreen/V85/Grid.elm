module Evergreen.V85.Grid exposing (..)

import Dict
import Evergreen.V85.Ascii
import Evergreen.V85.GridCell
import Evergreen.V85.Helper
import Evergreen.V85.Units
import Evergreen.V85.User
import List.Nonempty
import Math.Vector2


type alias LocalGridChange =
    { cellPosition : Evergreen.V85.Helper.Coord Evergreen.V85.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V85.Ascii.Ascii
    }


type alias GridChange =
    { cellPosition : Evergreen.V85.Helper.Coord Evergreen.V85.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V85.Ascii.Ascii
    , userId : Evergreen.V85.User.UserId
    }


type Grid
    = Grid (Dict.Dict ( Int, Int ) Evergreen.V85.GridCell.Cell)


type alias Vertex =
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }
