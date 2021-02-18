module Env exposing (Mode(..), adminEmail, adminEmail_, adminUserId, adminUserId_, confirmationEmailKey, domain, hyperlinkWhitelist, isProduction, isProduction_, mapDrawAt, mapDrawAtX, mapDrawAtY, mode, notifyAdminWaitInHours, sendGridKey, sendGridKey_, startPointAt, startPointX, startPointY, statisticsBounds, statisticsDrawAt, statisticsDrawAtX, statisticsDrawAtY, statisticsX0, statisticsX1, statisticsY0, statisticsY1)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.

import Bounds exposing (Bounds)
import Duration exposing (Duration)
import Email
import Helper exposing (Coord)
import SendGrid
import Units exposing (AsciiUnit)
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


adminEmail_ : String
adminEmail_ =
    ""


adminEmail : Email.Email
adminEmail =
    Email.fromString adminEmail_
        |> Maybe.withDefault
            { localPart = ""
            , tags = []
            , domain = ""
            , tld = []
            }


sendGridKey_ : String
sendGridKey_ =
    ""


sendGridKey : SendGrid.ApiKey
sendGridKey =
    SendGrid.apiKey sendGridKey_


domain : String
domain =
    "localhost:8000"


statisticsX0 : String
statisticsX0 =
    "-258"


statisticsY0 : String
statisticsY0 =
    "-78"


statisticsX1 : String
statisticsX1 =
    "1136"


statisticsY1 : String
statisticsY1 =
    "209"


statisticsBounds : Bounds AsciiUnit
statisticsBounds =
    Maybe.map4
        (\x0 y0 x1 y1 -> Bounds.bounds (Helper.fromRawCoord ( x0, y0 )) (Helper.fromRawCoord ( x1, y1 )))
        (String.toInt statisticsX0)
        (String.toInt statisticsY0)
        (String.toInt statisticsX1)
        (String.toInt statisticsY1)
        |> Maybe.withDefault (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 0, 0 )))


statisticsDrawAtX : String
statisticsDrawAtX =
    "32"


statisticsDrawAtY : String
statisticsDrawAtY =
    "0"


statisticsDrawAt : Coord AsciiUnit
statisticsDrawAt =
    Maybe.map2
        (\x y -> Helper.fromRawCoord ( x, y ))
        (String.toInt statisticsDrawAtX)
        (String.toInt statisticsDrawAtY)
        |> Maybe.withDefault (Helper.fromRawCoord ( 32, 0 ))


mapDrawAtX : String
mapDrawAtX =
    "759"


mapDrawAtY : String
mapDrawAtY =
    "-71"


mapDrawAt : Coord AsciiUnit
mapDrawAt =
    Maybe.map2
        (\x y -> Helper.fromRawCoord ( x, y ))
        (String.toInt mapDrawAtX)
        (String.toInt mapDrawAtY)
        |> Maybe.withDefault (Helper.fromRawCoord ( 32, 100 ))


notifyAdminWaitInHours : String
notifyAdminWaitInHours =
    "0.05"


hyperlinkWhitelist : String
hyperlinkWhitelist =
    "www.patorjk.com/software/taag, ro-box.netlify.app, the-best-color.lamdera.app, agirg.com, yourworldoftext.com, www.yourworldoftext.com"


confirmationEmailKey : String
confirmationEmailKey =
    "abc"


startPointX : String
startPointX =
    "123"


startPointY : String
startPointY =
    "44"


startPointAt : Coord AsciiUnit
startPointAt =
    Maybe.map2
        (\x y -> Helper.fromRawCoord ( x, y ))
        (String.toInt startPointX)
        (String.toInt startPointY)
        |> Maybe.withDefault (Helper.fromRawCoord ( 0, 0 ))
