module Evergreen.V83.RecentChanges exposing (..)

import Dict
import Evergreen.V83.GridCell
import Evergreen.V83.Helper
import Evergreen.V83.NotifyMe
import Quantity
import SeqDict


type RecentChanges
    = RecentChanges
        { frequencies : SeqDict.SeqDict Evergreen.V83.NotifyMe.Frequency (Dict.Dict Evergreen.V83.Helper.RawCellCoord Evergreen.V83.GridCell.Cell)
        , threeHoursElapsed : Quantity.Quantity Int Evergreen.V83.NotifyMe.ThreeHours
        }
