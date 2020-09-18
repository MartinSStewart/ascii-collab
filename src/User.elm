module User exposing (RawUserId, UserId(..), color, rawId, userId)

import Angle
import Basics.Extra as Basics
import ColorHelper
import Element
import Random


type UserId
    = UserId Int


type alias RawUserId =
    Int


userId : Int -> UserId
userId index =
    UserId index


rawId : UserId -> Int
rawId (UserId userId_) =
    userId_


color : UserId -> Element.Color
color (UserId userId_) =
    let
        userIdFloat =
            toFloat (userId_ + 6)
    in
    { hue = userIdFloat * 33 |> Angle.degrees
    , saturation = userIdFloat * 1.92 |> Basics.fractionalModBy 0.8 |> (+) 0.2
    , value = userIdFloat * 1.61 |> Basics.fractionalModBy 0.5 |> (+) 0.5
    }
        |> ColorHelper.hsvToRgb



--Random.map3 ColorHelper.Hsv
--    (Random.float 0 360 |> Random.map Angle.degrees)
--    (Random.float 0 1 |> Random.map ((*) 0.2 >> (+) 0.7))
--    (Random.float 0 1 |> Random.map ((*) 0.2 >> (+) 0.7))
--    |> (\hsvGen -> Random.step hsvGen (Random.initialSeed userId_))
--    |> Tuple.first
--    |> ColorHelper.hsvToRgb
