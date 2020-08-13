module Evergreen.V1.User exposing (..)

import Lamdera


type UserId
    = TempUserId Lamdera.SessionId