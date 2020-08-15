module Evergreen.V3.Grid exposing (..)

import Evergreen.V3.Ascii
import Dict
import Evergreen.V3.GridCell
import Evergreen.V3.Helper
import List.Nonempty
import Evergreen.V3.Units


type Grid
    = Grid (Dict.Dict (Int, Int) Evergreen.V3.GridCell.Cell)


type alias Change = 
    { cellPosition : (Evergreen.V3.Helper.Coord Evergreen.V3.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V3.Ascii.Ascii)
    }


type alias ChangeBroadcast = 
    { cellPosition : (Evergreen.V3.Helper.Coord Evergreen.V3.Units.CellUnit)
    , localPosition : Int
    , change : (List.Nonempty.Nonempty Evergreen.V3.Ascii.Ascii)
    , changeId : Int
    }