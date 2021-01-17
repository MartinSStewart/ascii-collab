module Evergreen.V47.RecentChanges exposing (..)

import AssocList
import Dict
import Evergreen.V47.GridCell
import Evergreen.V47.Helper
import Evergreen.V47.NotifyMe
import Quantity


type RecentChanges
    = RecentChanges 
    { frequencies : (AssocList.Dict Evergreen.V47.NotifyMe.Frequency (Dict.Dict Evergreen.V47.Helper.RawCellCoord Evergreen.V47.GridCell.Cell))
    , threeHoursElapsed : (Quantity.Quantity Int Evergreen.V47.NotifyMe.ThreeHours)
    }