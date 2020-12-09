module Evergreen.V45.Grid exposing (..)

import Dict
import Evergreen.V45.Ascii
import Evergreen.V45.GridCell
import Evergreen.V45.Helper
import Evergreen.V45.Units
import Evergreen.V45.User
import List.Nonempty
import Math.Vector2


type alias LocalChange = 
    { cellPosition : (Evergreen.V45.Helper.Coord Evergreen.V45.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V45.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V45.Helper.Coord Evergreen.V45.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V45.Ascii.Ascii)
    , userId : Evergreen.V45.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V45.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }