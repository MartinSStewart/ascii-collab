module Evergreen.V3.User exposing (..)

import Lamdera


type UserId
    = TempUserId Lamdera.SessionId