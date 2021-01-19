module Evergreen.V47.Grid exposing (..)

import Dict
import Evergreen.V47.Ascii
import Evergreen.V47.GridCell
import Evergreen.V47.Helper
import Evergreen.V47.Units
import Evergreen.V47.User
import List.Nonempty
import Math.Vector2


type alias LocalGridChange = 
    { cellPosition : (Evergreen.V47.Helper.Coord Evergreen.V47.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V47.Ascii.Ascii)
    }


type alias GridChange = 
    { cellPosition : (Evergreen.V47.Helper.Coord Evergreen.V47.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V47.Ascii.Ascii)
    , userId : Evergreen.V47.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V47.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }