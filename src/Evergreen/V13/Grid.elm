module Evergreen.V13.Grid exposing (..)

import Evergreen.V13.Ascii
import Dict
import Evergreen.V13.GridCell
import Evergreen.V13.Helper
import List.Nonempty
import Math.Vector2
import Evergreen.V13.Units
import Evergreen.V13.User


type alias LocalChange = 
    { cellPosition : (Evergreen.V13.Helper.Coord Evergreen.V13.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V13.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V13.Helper.Coord Evergreen.V13.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V13.Ascii.Ascii)
    , userId : Evergreen.V13.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V13.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , userId : Float
    }