module Evergreen.V80.RecentChanges exposing (..)

import AssocList
import Dict
import Evergreen.V80.GridCell
import Evergreen.V80.Helper
import Evergreen.V80.NotifyMe
import Quantity


type RecentChanges
    = RecentChanges
        { frequencies : AssocList.Dict Evergreen.V80.NotifyMe.Frequency (Dict.Dict Evergreen.V80.Helper.RawCellCoord Evergreen.V80.GridCell.Cell)
        , threeHoursElapsed : Quantity.Quantity Int Evergreen.V80.NotifyMe.ThreeHours
        }
