module Evergreen.V85.RecentChanges exposing (..)

import Dict
import Evergreen.V85.GridCell
import Evergreen.V85.Helper
import Evergreen.V85.NotifyMe
import Quantity
import SeqDict


type RecentChanges
    = RecentChanges
        { frequencies : SeqDict.SeqDict Evergreen.V85.NotifyMe.Frequency (Dict.Dict Evergreen.V85.Helper.RawCellCoord Evergreen.V85.GridCell.Cell)
        , threeHoursElapsed : Quantity.Quantity Int Evergreen.V85.NotifyMe.ThreeHours
        }
