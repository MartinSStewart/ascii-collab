module Evergreen.V38.Grid exposing (..)

import Dict
import Evergreen.V38.Ascii
import Evergreen.V38.GridCell
import Evergreen.V38.Helper
import Evergreen.V38.Units
import Evergreen.V38.User
import List.Nonempty
import Math.Vector2


type alias LocalChange = 
    { cellPosition : (Evergreen.V38.Helper.Coord Evergreen.V38.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V38.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V38.Helper.Coord Evergreen.V38.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V38.Ascii.Ascii)
    , userId : Evergreen.V38.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V38.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }