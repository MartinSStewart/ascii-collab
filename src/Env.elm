module Env exposing (..)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.

import User exposing (UserId)


adminUserId_ =
    "0"


adminUserId : Maybe UserId
adminUserId =
    String.toInt adminUserId_ |> Maybe.map User.userId
