module Evergreen.V83.NotifyMe exposing (..)

import Evergreen.V83.EmailAddress2


type Status
    = Form
    | FormWithError
    | SendingToBackend
    | WaitingOnConfirmation


type Frequency
    = Every3Hours
    | Every12Hours
    | Daily
    | Weekly
    | Monthly


type alias InProgressModel =
    { status : Status
    , email : String
    , frequency : Maybe Frequency
    }


type Model
    = InProgress InProgressModel
    | Completed
    | BackendError
    | Unsubscribing
    | Unsubscribed


type ThreeHours
    = ThreeHours Never


type alias Validated =
    { email : Evergreen.V83.EmailAddress2.EmailAddress
    , frequency : Frequency
    }
