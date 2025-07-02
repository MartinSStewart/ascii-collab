module Evergreen.V81.RecentChanges exposing (..)

import Dict
import Evergreen.V81.GridCell
import Evergreen.V81.Helper
import Evergreen.V81.NotifyMe
import Quantity
import SeqDict


type RecentChanges
    = RecentChanges
        { frequencies : SeqDict.SeqDict Evergreen.V81.NotifyMe.Frequency (Dict.Dict Evergreen.V81.Helper.RawCellCoord Evergreen.V81.GridCell.Cell)
        , threeHoursElapsed : Quantity.Quantity Int Evergreen.V81.NotifyMe.ThreeHours
        }
