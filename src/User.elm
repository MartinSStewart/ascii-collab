module User exposing (RawUserId, UserId(..), color, rawId, userId)

import Angle
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
    Random.map3 ColorHelper.Hsv
        (Random.float 0 360 |> Random.map Angle.degrees)
        (Random.float 0 1 |> Random.map ((*) 0.2 >> (+) 0.7))
        (Random.float 0 1 |> Random.map ((*) 0.2 >> (+) 0.7))
        |> (\hsvGen -> Random.step hsvGen (Random.initialSeed userId_))
        |> Tuple.first
        |> ColorHelper.hsvToRgb
