module Evergreen.V79.RecentChanges exposing (..)

import AssocList
import Dict
import Evergreen.V79.GridCell
import Evergreen.V79.Helper
import Evergreen.V79.NotifyMe
import Quantity


type RecentChanges
    = RecentChanges
        { frequencies : AssocList.Dict Evergreen.V79.NotifyMe.Frequency (Dict.Dict Evergreen.V79.Helper.RawCellCoord Evergreen.V79.GridCell.Cell)
        , threeHoursElapsed : Quantity.Quantity Int Evergreen.V79.NotifyMe.ThreeHours
        }
