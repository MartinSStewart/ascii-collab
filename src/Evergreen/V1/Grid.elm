module Evergreen.V1.Grid exposing (..)

import Evergreen.V1.Ascii
import Dict
import Evergreen.V1.GridCell
import Evergreen.V1.Helper
import List.Nonempty
import Evergreen.V1.Units
import Evergreen.V1.User


type alias LocalChange = 
    { cellPosition : (Evergreen.V1.Helper.Coord Evergreen.V1.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V1.Ascii.Ascii)
    }


type alias Change = 
    { cellPosition : (Evergreen.V1.Helper.Coord Evergreen.V1.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V1.Ascii.Ascii)
    , userId : Evergreen.V1.User.UserId
    }


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V1.GridCell.Cell)