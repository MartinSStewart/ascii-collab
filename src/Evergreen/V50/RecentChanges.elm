module Evergreen.V50.RecentChanges exposing (..)

import AssocList
import Dict
import Evergreen.V50.GridCell
import Evergreen.V50.Helper
import Evergreen.V50.NotifyMe
import Quantity


type RecentChanges
    = RecentChanges 
    { frequencies : (AssocList.Dict Evergreen.V50.NotifyMe.Frequency (Dict.Dict Evergreen.V50.Helper.RawCellCoord Evergreen.V50.GridCell.Cell))
    , threeHoursElapsed : (Quantity.Quantity Int Evergreen.V50.NotifyMe.ThreeHours)
    }