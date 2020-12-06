module Evergreen.V39.Grid exposing (..)

import Dict
import Evergreen.V39.Ascii
import Evergreen.V39.GridCell
import Evergreen.V39.Helper
import Evergreen.V39.Units
import Evergreen.V39.User
import List.Nonempty
import Math.Vector2


type alias LocalChange = 
    { cellPosition : (Evergreen.V39.Helper.Coord Evergreen.V39.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V39.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V39.Helper.Coord Evergreen.V39.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V39.Ascii.Ascii)
    , userId : Evergreen.V39.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V39.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }