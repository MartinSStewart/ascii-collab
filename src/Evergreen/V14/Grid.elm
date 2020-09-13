module Evergreen.V14.Grid exposing (..)

import Evergreen.V14.Ascii
import Dict
import Evergreen.V14.GridCell
import Evergreen.V14.Helper
import List.Nonempty
import Math.Vector2
import Evergreen.V14.Units
import Evergreen.V14.User


type alias LocalChange = 
    { cellPosition : (Evergreen.V14.Helper.Coord Evergreen.V14.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V14.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V14.Helper.Coord Evergreen.V14.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V14.Ascii.Ascii)
    , userId : Evergreen.V14.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V14.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , userId : Float
    }