module Evergreen.V17.Grid exposing (..)

import Evergreen.V17.Ascii
import Dict
import Evergreen.V17.GridCell
import Evergreen.V17.Helper
import List.Nonempty
import Math.Vector2
import Evergreen.V17.Units
import Evergreen.V17.User


type alias LocalChange = 
    { cellPosition : (Evergreen.V17.Helper.Coord Evergreen.V17.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V17.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V17.Helper.Coord Evergreen.V17.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V17.Ascii.Ascii)
    , userId : Evergreen.V17.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V17.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , userId : Float
    }