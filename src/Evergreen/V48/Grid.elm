module Evergreen.V48.Grid exposing (..)

import Dict
import Evergreen.V48.Ascii
import Evergreen.V48.GridCell
import Evergreen.V48.Helper
import Evergreen.V48.Units
import Evergreen.V48.User
import List.Nonempty
import Math.Vector2


type alias LocalGridChange = 
    { cellPosition : (Evergreen.V48.Helper.Coord Evergreen.V48.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V48.Ascii.Ascii)
    }


type alias GridChange = 
    { cellPosition : (Evergreen.V48.Helper.Coord Evergreen.V48.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V48.Ascii.Ascii)
    , userId : Evergreen.V48.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V48.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }