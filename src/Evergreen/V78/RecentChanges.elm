module Evergreen.V78.RecentChanges exposing (..)

import AssocList
import Dict
import Evergreen.V78.GridCell
import Evergreen.V78.Helper
import Evergreen.V78.NotifyMe
import Quantity


type RecentChanges
    = RecentChanges
        { frequencies : AssocList.Dict Evergreen.V78.NotifyMe.Frequency (Dict.Dict Evergreen.V78.Helper.RawCellCoord Evergreen.V78.GridCell.Cell)
        , threeHoursElapsed : Quantity.Quantity Int Evergreen.V78.NotifyMe.ThreeHours
        }
