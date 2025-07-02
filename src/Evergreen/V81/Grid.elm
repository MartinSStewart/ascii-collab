module Evergreen.V81.Grid exposing (..)

import Dict
import Evergreen.V81.Ascii
import Evergreen.V81.GridCell
import Evergreen.V81.Helper
import Evergreen.V81.Units
import Evergreen.V81.User
import List.Nonempty
import Math.Vector2


type alias LocalGridChange =
    { cellPosition : Evergreen.V81.Helper.Coord Evergreen.V81.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V81.Ascii.Ascii
    }


type alias GridChange =
    { cellPosition : Evergreen.V81.Helper.Coord Evergreen.V81.Units.CellUnit
    , localPosition : Int
    , change : List.Nonempty.Nonempty Evergreen.V81.Ascii.Ascii
    , userId : Evergreen.V81.User.UserId
    }


type Grid
    = Grid (Dict.Dict ( Int, Int ) Evergreen.V81.GridCell.Cell)


type alias Vertex =
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }
