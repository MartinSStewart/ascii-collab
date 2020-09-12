module Env exposing (..)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.

import SendGrid
import User exposing (UserId)


adminUserId_ : String
adminUserId_ =
    "0"


adminUserId : Maybe UserId
adminUserId =
    String.toInt adminUserId_ |> Maybe.map User.userId


isProduction_ : String
isProduction_ =
    "False"


isProduction : Bool
isProduction =
    case String.toLower isProduction_ |> String.trim of
        "true" ->
            True

        "false" ->
            False

        _ ->
            False


adminEmail : String
adminEmail =
    ""


sendGridKey_ : String
sendGridKey_ =
    ""


sendGridKey : SendGrid.ApiKey
sendGridKey =
    SendGrid.apiKey sendGridKey_


domain : String
domain =
    "localhost:8000"
