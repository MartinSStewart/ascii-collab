module Evergreen.V78.Grid exposing (..)

import Dict
import Evergreen.V78.Ascii
import Evergreen.V78.GridCell
import Evergreen.V78.Helper
import Evergreen.V78.Units
import Evergreen.V78.User
import List.Nonempty
import Math.Vector2


type alias LocalGridChange =
    { cellPosition : Evergreen.V78.Helper.Coord Evergreen.V78.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V78.Ascii.Ascii
    }


type alias GridChange =
    { cellPosition : Evergreen.V78.Helper.Coord Evergreen.V78.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V78.Ascii.Ascii
    , userId : Evergreen.V78.User.UserId
    }


type Grid
    = Grid (Dict.Dict ( Int, Int ) Evergreen.V78.GridCell.Cell)


type alias Vertex =
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }
