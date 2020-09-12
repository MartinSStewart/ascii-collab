module Evergreen.V12.Grid exposing (..)

import Evergreen.V12.Ascii
import Dict
import Evergreen.V12.GridCell
import Evergreen.V12.Helper
import List.Nonempty
import Math.Vector2
import Evergreen.V12.Units
import Evergreen.V12.User


type alias LocalChange = 
    { cellPosition : (Evergreen.V12.Helper.Coord Evergreen.V12.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V12.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V12.Helper.Coord Evergreen.V12.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V12.Ascii.Ascii)
    , userId : Evergreen.V12.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V12.GridCell.Cell)


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    , userId : Float
    }