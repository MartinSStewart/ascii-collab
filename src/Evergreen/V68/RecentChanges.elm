module Evergreen.V68.RecentChanges exposing (..)

import AssocList
import Dict
import Evergreen.V68.GridCell
import Evergreen.V68.Helper
import Evergreen.V68.NotifyMe
import Quantity


type RecentChanges
    = RecentChanges
        { frequencies : AssocList.Dict Evergreen.V68.NotifyMe.Frequency (Dict.Dict Evergreen.V68.Helper.RawCellCoord Evergreen.V68.GridCell.Cell)
        , threeHoursElapsed : Quantity.Quantity Int Evergreen.V68.NotifyMe.ThreeHours
        }
