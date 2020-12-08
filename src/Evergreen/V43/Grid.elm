module Evergreen.V43.Grid exposing (..)

import Dict
import Evergreen.V43.Ascii
import Evergreen.V43.GridCell
import Evergreen.V43.Helper
import Evergreen.V43.Units
import Evergreen.V43.User
import List.Nonempty
import Math.Vector2


type alias LocalChange = 
    { cellPosition : (Evergreen.V43.Helper.Coord Evergreen.V43.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V43.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V43.Helper.Coord Evergreen.V43.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V43.Ascii.Ascii)
    , userId : Evergreen.V43.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V43.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }