module ColorIndex exposing (ColorIndex(..), colorToVec3, colors, toColor)

import Element
import Math.Vector3


type ColorIndex
    = Green
    | Teal
    | Blue
    | Purple
    | Magenta
    | Salmon
    | Orange
    | Yellow
    | Gray


colors : List ColorIndex
colors =
    [ Green
    , Teal
    , Blue
    , Purple
    , Magenta
    , Salmon
    , Orange
    , Yellow
    , Gray
    ]


toColor : ColorIndex -> Element.Color
toColor colorIndex =
    case colorIndex of
        Green ->
            Element.rgb255 98 218 0

        Teal ->
            Element.rgb255 0 210 175

        Blue ->
            Element.rgb255 125 168 255

        Purple ->
            Element.rgb255 191 128 255

        Magenta ->
            Element.rgb255 255 72 225

        Salmon ->
            Element.rgb255 255 134 136

        Orange ->
            Element.rgb255 255 152 49

        Yellow ->
            Element.rgb255 203 186 0

        Gray ->
            Element.rgb255 179 179 179


colorToVec3 : Element.Color -> Math.Vector3.Vec3
colorToVec3 color =
    let
        { red, green, blue } =
            Element.toRgb color
    in
    Math.Vector3.vec3 red green blue
