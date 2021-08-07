module Evergreen.V73.RecentChanges exposing (..)

import AssocList
import Dict
import Evergreen.V73.GridCell
import Evergreen.V73.Helper
import Evergreen.V73.NotifyMe
import Quantity


type RecentChanges
    = RecentChanges
        { frequencies : AssocList.Dict Evergreen.V73.NotifyMe.Frequency (Dict.Dict Evergreen.V73.Helper.RawCellCoord Evergreen.V73.GridCell.Cell)
        , threeHoursElapsed : Quantity.Quantity Int Evergreen.V73.NotifyMe.ThreeHours
        }
