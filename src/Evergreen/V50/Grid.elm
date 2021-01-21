module Evergreen.V50.Grid exposing (..)

import Dict
import Evergreen.V50.Ascii
import Evergreen.V50.GridCell
import Evergreen.V50.Helper
import Evergreen.V50.Units
import Evergreen.V50.User
import List.Nonempty
import Math.Vector2


type alias LocalGridChange = 
    { cellPosition : (Evergreen.V50.Helper.Coord Evergreen.V50.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V50.Ascii.Ascii)
    }


type alias GridChange = 
    { cellPosition : (Evergreen.V50.Helper.Coord Evergreen.V50.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V50.Ascii.Ascii)
    , userId : Evergreen.V50.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V50.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , quadPosition : Math.Vector2.Vec2
    , userId : Float
    }