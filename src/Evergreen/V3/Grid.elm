module Evergreen.V3.Grid exposing (..)

import Evergreen.V3.Ascii
import Dict
import Evergreen.V3.GridCell
import Evergreen.V3.Helper
import List.Nonempty
import Evergreen.V3.Units
import Evergreen.V3.User


type alias LocalChange = 
    { cellPosition : (Evergreen.V3.Helper.Coord Evergreen.V3.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V3.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V3.Helper.Coord Evergreen.V3.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V3.Ascii.Ascii)
    , userId : Evergreen.V3.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V3.GridCell.Cell)