module Evergreen.V50.NotifyMe exposing (..)

import Email


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
    , frequency : (Maybe Frequency)
    }


type Model
    = InProgress InProgressModel
    | Completed
    | BackendError
    | Unsubscribed


type ThreeHours
    = ThreeHours Never


type alias Validated = 
    { email : Email.Email
    , frequency : Frequency
    }