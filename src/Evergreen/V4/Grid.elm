module Evergreen.V4.Grid exposing (..)

import Evergreen.V4.Ascii
import Dict
import Evergreen.V4.GridCell
import Evergreen.V4.Helper
import List.Nonempty
import Math.Vector2
import Evergreen.V4.Units
import Evergreen.V4.User


type alias LocalChange = 
    { cellPosition : (Evergreen.V4.Helper.Coord Evergreen.V4.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V4.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V4.Helper.Coord Evergreen.V4.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V4.Ascii.Ascii)
    , userId : Evergreen.V4.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V4.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , userId : Float
    }