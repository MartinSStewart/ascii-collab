module Evergreen.V77.RecentChanges exposing (..)

import AssocList
import Dict
import Evergreen.V77.GridCell
import Evergreen.V77.Helper
import Evergreen.V77.NotifyMe
import Quantity


type RecentChanges
    = RecentChanges
        { frequencies : AssocList.Dict Evergreen.V77.NotifyMe.Frequency (Dict.Dict Evergreen.V77.Helper.RawCellCoord Evergreen.V77.GridCell.Cell)
        , threeHoursElapsed : Quantity.Quantity Int Evergreen.V77.NotifyMe.ThreeHours
        }
