module Evergreen.V28.Grid exposing (..)

import Evergreen.V28.Ascii
import Dict
import Evergreen.V28.GridCell
import Evergreen.V28.Helper
import List.Nonempty
import Math.Vector2
import Evergreen.V28.Units
import Evergreen.V28.User


type alias LocalChange = 
    { cellPosition : (Evergreen.V28.Helper.Coord Evergreen.V28.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V28.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V28.Helper.Coord Evergreen.V28.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V28.Ascii.Ascii)
    , userId : Evergreen.V28.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V28.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , userId : Float
    }