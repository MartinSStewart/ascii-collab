module Evergreen.V53.RecentChanges exposing (..)

import AssocList
import Dict
import Evergreen.V53.GridCell
import Evergreen.V53.Helper
import Evergreen.V53.NotifyMe
import Quantity


type RecentChanges
    = RecentChanges 
    { frequencies : (AssocList.Dict Evergreen.V53.NotifyMe.Frequency (Dict.Dict Evergreen.V53.Helper.RawCellCoord Evergreen.V53.GridCell.Cell))
    , threeHoursElapsed : (Quantity.Quantity Int Evergreen.V53.NotifyMe.ThreeHours)
    }