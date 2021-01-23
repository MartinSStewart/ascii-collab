module Evergreen.V53.Grid exposing (..)

import Dict
import Evergreen.V53.Ascii
import Evergreen.V53.GridCell
import Evergreen.V53.Helper
import Evergreen.V53.Units
import Evergreen.V53.User
import List.Nonempty
import Math.Vector2


type alias LocalGridChange = 
    { cellPosition : (Evergreen.V53.Helper.Coord Evergreen.V53.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V53.Ascii.Ascii)
    }


type alias GridChange = 
    { cellPosition : (Evergreen.V53.Helper.Coord Evergreen.V53.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V53.Ascii.Ascii)
    , userId : Evergreen.V53.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V53.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }