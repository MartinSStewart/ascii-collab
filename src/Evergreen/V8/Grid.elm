module Evergreen.V8.Grid exposing (..)

import Evergreen.V8.Ascii
import Dict
import Evergreen.V8.GridCell
import Evergreen.V8.Helper
import List.Nonempty
import Math.Vector2
import Evergreen.V8.Units
import Evergreen.V8.User


type alias LocalChange = 
    { cellPosition : (Evergreen.V8.Helper.Coord Evergreen.V8.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V8.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V8.Helper.Coord Evergreen.V8.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V8.Ascii.Ascii)
    , userId : Evergreen.V8.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V8.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , userId : Float
    }