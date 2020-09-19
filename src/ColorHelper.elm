module ColorHelper exposing (Hsv, colorToVec3, hsvToRgb, rgbToHsv)

import Angle exposing (Angle)
import Basics.Extra as Basics
import Element
import Math.Vector3


type alias Hsv =
    { hue : Angle, saturation : Float, value : Float }


rgbToHsv : Element.Color -> Hsv
rgbToHsv color =
    let
        { red, green, blue } =
            Element.toRgb color

        cMax =
            max red green |> max blue

        cMin =
            min red green |> min blue

        delta =
            cMax - cMin
    in
    { hue =
        (if red == cMax then
            (green - blue) / delta |> Basics.fractionalModBy 6

         else if green == cMax then
            (blue - red) / delta |> (+) 2

         else
            (red - green) / delta |> (+) 4
        )
            |> (*) 60
            |> nanOrInfDefaultsTo 0
            |> Angle.degrees
    , saturation = nanOrInfDefaultsTo 0 (delta / cMax)
    , value = cMax
    }


nanOrInfDefaultsTo : Float -> Float -> Float
nanOrInfDefaultsTo default value =
    if isNaN value || isInfinite value then
        default

    else
        value


hsvToRgb : Hsv -> Element.Color
hsvToRgb { hue, saturation, value } =
    let
        hueDegrees =
            Angle.inDegrees hue |> Basics.fractionalModBy 360

        c =
            value * saturation

        x =
            c * (hueDegrees / 60 |> Basics.fractionalModBy 2 |> (+) -1 |> abs |> (-) 1)

        m =
            value - c
    in
    (if hueDegrees < 60 then
        ( c, x, 0 )

     else if hueDegrees < 120 then
        ( x, c, 0 )

     else if hueDegrees < 180 then
        ( 0, c, x )

     else if hueDegrees < 240 then
        ( 0, x, c )

     else if hueDegrees < 300 then
        ( x, 0, c )

     else
        ( c, 0, x )
    )
        |> (\( r, g, b ) ->
                Element.rgb (r + m) (g + m) (b + m)
           )
