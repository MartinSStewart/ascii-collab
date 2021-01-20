module Evergreen.V48.RecentChanges exposing (..)

import AssocList
import Dict
import Evergreen.V48.GridCell
import Evergreen.V48.Helper
import Evergreen.V48.NotifyMe
import Quantity


type RecentChanges
    = RecentChanges 
    { frequencies : (AssocList.Dict Evergreen.V48.NotifyMe.Frequency (Dict.Dict Evergreen.V48.Helper.RawCellCoord Evergreen.V48.GridCell.Cell))
    , threeHoursElapsed : (Quantity.Quantity Int Evergreen.V48.NotifyMe.ThreeHours)
    }