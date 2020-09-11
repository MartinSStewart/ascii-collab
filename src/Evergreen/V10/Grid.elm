module Evergreen.V10.Grid exposing (..)

import Evergreen.V10.Ascii
import Dict
import Evergreen.V10.GridCell
import Evergreen.V10.Helper
import List.Nonempty
import Math.Vector2
import Evergreen.V10.Units
import Evergreen.V10.User


type alias LocalChange = 
    { cellPosition : (Evergreen.V10.Helper.Coord Evergreen.V10.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V10.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V10.Helper.Coord Evergreen.V10.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V10.Ascii.Ascii)
    , userId : Evergreen.V10.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V10.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , userId : Float
    }