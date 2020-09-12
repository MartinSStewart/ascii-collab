module ColorIndex exposing (ColorIndex(..), colorToVec3, colors, toColor)

import Element
import Math.Vector3


type ColorIndex
    = Green
    | DarkGreen
    | Teal
    | DarkTeal
    | Blue
    | DarkBlue
    | Purple
    | DarkPurple
    | Magenta
    | DarkMagenta
    | Salmon
    | DarkSalmon
    | Orange
    | DarkOrange
    | Yellow
    | DarkYellow
    | Gray
    | DarkGray


colors : List ColorIndex
colors =
    [ Green
    , DarkGreen
    , Teal
    , DarkTeal
    , Blue
    , DarkBlue
    , Purple
    , DarkPurple
    , Magenta
    , DarkMagenta
    , Salmon
    , DarkSalmon
    , Orange
    , DarkOrange
    , Yellow
    , DarkYellow
    , Gray
    , DarkGray
    ]


darken : Element.Color -> Element.Color
darken color =
    let
        { red, green, blue } =
            Element.toRgb color

        darken_ =
            (*) 0.8
    in
    Element.rgb (darken_ red) (darken_ green) (darken_ blue)


toColor : ColorIndex -> Element.Color
toColor colorIndex =
    case colorIndex of
        Green ->
            Element.rgb255 98 218 0

        DarkGreen ->
            Element.rgb255 98 218 0 |> darken

        Teal ->
            Element.rgb255 0 210 175

        DarkTeal ->
            Element.rgb255 0 210 175 |> darken

        Blue ->
            Element.rgb255 125 168 255

        DarkBlue ->
            Element.rgb255 125 168 255 |> darken

        Purple ->
            Element.rgb255 191 128 255

        DarkPurple ->
            Element.rgb255 191 128 255 |> darken

        Magenta ->
            Element.rgb255 255 72 225

        DarkMagenta ->
            Element.rgb255 255 72 225 |> darken

        Salmon ->
            Element.rgb255 255 134 136

        DarkSalmon ->
            Element.rgb255 255 134 136 |> darken

        Orange ->
            Element.rgb255 255 152 49

        DarkOrange ->
            Element.rgb255 255 152 49 |> darken

        Yellow ->
            Element.rgb255 203 186 0

        DarkYellow ->
            Element.rgb255 203 186 0 |> darken

        Gray ->
            Element.rgb255 179 179 179

        DarkGray ->
            Element.rgb255 179 179 179 |> darken


colorToVec3 : Element.Color -> Math.Vector3.Vec3
colorToVec3 color =
    let
        { red, green, blue } =
            Element.toRgb color
    in
    Math.Vector3.vec3 red green blue
