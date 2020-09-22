module Evergreen.V24.Grid exposing (..)

import Evergreen.V24.Ascii
import Dict
import Evergreen.V24.GridCell
import Evergreen.V24.Helper
import List.Nonempty
import Math.Vector2
import Evergreen.V24.Units
import Evergreen.V24.User


type alias LocalChange = 
    { cellPosition : (Evergreen.V24.Helper.Coord Evergreen.V24.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V24.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V24.Helper.Coord Evergreen.V24.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V24.Ascii.Ascii)
    , userId : Evergreen.V24.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V24.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , userId : Float
    }