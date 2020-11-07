module Ascii exposing (Ascii(..), asciiChars, asciis, charsPerRow, default, fromChar, fromInt, intensity, size, textureData, texturePosition, texturePositionInt, toChar, toInt)

import Bounds exposing (Bounds)
import Dict exposing (Dict)
import Helper exposing (Coord)
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import Math.Vector2 exposing (Vec2)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)


asciiChars : List Char
asciiChars =
    (List.range 32 126 ++ List.range 161 172 ++ List.range 174 255)
        |> List.map Char.fromCode
        |> (++) [ '░', '▒', '▓', '█' ]
        |> (++) [ '│', '┤', '╡', '╢', '╖', '╕', '╣', '║', '╗', '╝', '╜', '╛', '┐', '└', '┴', '┬', '├', '─', '┼', '╞', '╟', '╚', '╔', '╩', '╦', '╠', '═', '╬', '╧', '╨', '╤', '╥', '╙', '╘', '╒', '╓', '╫', '╪', '┘', '┌' ]


asciis : Nonempty Ascii
asciis =
    List.filterMap fromChar asciiChars
        |> List.Nonempty.fromList
        |> Maybe.withDefault (List.Nonempty.fromElement default)


charToAscii : Dict Char Ascii
charToAscii =
    asciiChars |> List.indexedMap (\index char -> ( char, Ascii index )) |> Dict.fromList


asciiToChar : Dict Int Char
asciiToChar =
    asciiChars |> List.indexedMap (\index char -> ( index, char )) |> Dict.fromList


fromChar : Char -> Maybe Ascii
fromChar char =
    Dict.get char charToAscii


toChar : Ascii -> Char
toChar (Ascii ascii_) =
    Dict.get ascii_ asciiToChar |> Maybe.withDefault ' '


asciiCharCount : Int
asciiCharCount =
    List.length asciiChars


size : ( Quantity number Pixels, Quantity number Pixels )
size =
    ( Pixels.pixels 10, Pixels.pixels 18 )


default : Ascii
default =
    asciiChars |> List.findIndex ((==) ' ') |> Maybe.withDefault 0 |> Ascii


charsPerRow : number
charsPerRow =
    25


textureData : String
textureData =
    "data:image/png;base64,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"


textureSize : Quantity number Pixels
textureSize =
    Pixels.pixels 256


type Ascii
    = Ascii Int


toInt : Ascii -> Int
toInt (Ascii ascii) =
    ascii


fromInt : Int -> Maybe Ascii
fromInt value =
    if value >= 0 && value < asciiCharCount then
        Ascii value |> Just

    else
        Nothing


texturePosition : Ascii -> { topLeft : Vec2, bottomRight : Vec2 }
texturePosition (Ascii ascii_) =
    let
        ( Quantity.Quantity w, Quantity.Quantity h ) =
            size
    in
    { topLeft =
        Math.Vector2.vec2
            (modBy charsPerRow ascii_ |> (*) w |> toFloat |> (\a -> a / Pixels.inPixels textureSize))
            (ascii_ // charsPerRow |> (*) h |> toFloat |> (\a -> a / Pixels.inPixels textureSize))
    , bottomRight =
        Math.Vector2.vec2
            (modBy charsPerRow ascii_ |> (+) 1 |> (*) w |> toFloat |> (\a -> a / Pixels.inPixels textureSize))
            (ascii_ // charsPerRow |> (+) 1 |> (*) h |> toFloat |> (\a -> a / Pixels.inPixels textureSize))
    }


texturePositionInt : Ascii -> Bounds Pixels
texturePositionInt (Ascii ascii_) =
    let
        ( Quantity.Quantity w, Quantity.Quantity h ) =
            size
    in
    Bounds.bounds
        (Helper.fromRawCoord
            ( w * modBy charsPerRow ascii_
            , h * (ascii_ // charsPerRow)
            )
        )
        (Helper.fromRawCoord
            ( w * (modBy charsPerRow ascii_ + 1)
            , h * ((ascii_ // charsPerRow) + 1)
            )
        )


{-| Generated with <http://localhost:8000/tools/AsciiItensity.elm>
-}
intensity : Ascii -> Int
intensity ascii =
    case ascii of
        Ascii 232 ->
            26

        Ascii 231 ->
            31

        Ascii 230 ->
            27

        Ascii 229 ->
            24

        Ascii 228 ->
            25

        Ascii 227 ->
            23

        Ascii 226 ->
            23

        Ascii 225 ->
            24

        Ascii 224 ->
            9

        Ascii 223 ->
            18

        Ascii 222 ->
            22

        Ascii 221 ->
            21

        Ascii 220 ->
            19

        Ascii 219 ->
            19

        Ascii 218 ->
            28

        Ascii 217 ->
            30

        Ascii 216 ->
            17

        Ascii 215 ->
            20

        Ascii 214 ->
            18

        Ascii 213 ->
            18

        Ascii 212 ->
            23

        Ascii 211 ->
            26

        Ascii 210 ->
            24

        Ascii 209 ->
            24

        Ascii 208 ->
            25

        Ascii 207 ->
            34

        Ascii 206 ->
            35

        Ascii 205 ->
            26

        Ascii 204 ->
            30

        Ascii 203 ->
            29

        Ascii 202 ->
            27

        Ascii 201 ->
            27

        Ascii 200 ->
            25

        Ascii 199 ->
            26

        Ascii 198 ->
            24

        Ascii 197 ->
            27

        Ascii 196 ->
            29

        Ascii 195 ->
            27

        Ascii 194 ->
            27

        Ascii 193 ->
            30

        Ascii 192 ->
            12

        Ascii 191 ->
            24

        Ascii 190 ->
            28

        Ascii 189 ->
            27

        Ascii 188 ->
            25

        Ascii 187 ->
            25

        Ascii 186 ->
            38

        Ascii 185 ->
            32

        Ascii 184 ->
            23

        Ascii 183 ->
            26

        Ascii 182 ->
            24

        Ascii 181 ->
            24

        Ascii 180 ->
            32

        Ascii 179 ->
            35

        Ascii 178 ->
            33

        Ascii 177 ->
            33

        Ascii 176 ->
            28

        Ascii 175 ->
            38

        Ascii 174 ->
            35

        Ascii 173 ->
            30

        Ascii 172 ->
            34

        Ascii 171 ->
            33

        Ascii 170 ->
            31

        Ascii 169 ->
            31

        Ascii 168 ->
            16

        Ascii 167 ->
            33

        Ascii 166 ->
            30

        Ascii 165 ->
            35

        Ascii 164 ->
            16

        Ascii 163 ->
            12

        Ascii 162 ->
            11

        Ascii 161 ->
            7

        Ascii 160 ->
            4

        Ascii 159 ->
            37

        Ascii 158 ->
            24

        Ascii 157 ->
            3

        Ascii 156 ->
            12

        Ascii 155 ->
            11

        Ascii 154 ->
            22

        Ascii 153 ->
            8

        Ascii 152 ->
            10

        Ascii 151 ->
            46

        Ascii 150 ->
            18

        Ascii 149 ->
            16

        Ascii 148 ->
            15

        Ascii 147 ->
            38

        Ascii 146 ->
            8

        Ascii 145 ->
            38

        Ascii 144 ->
            10

        Ascii 143 ->
            33

        Ascii 142 ->
            12

        Ascii 141 ->
            21

        Ascii 140 ->
            17

        Ascii 139 ->
            12

        Ascii 138 ->
            7

        Ascii 137 ->
            11

        Ascii 136 ->
            12

        Ascii 135 ->
            11

        Ascii 134 ->
            21

        Ascii 133 ->
            24

        Ascii 132 ->
            24

        Ascii 131 ->
            25

        Ascii 130 ->
            19

        Ascii 129 ->
            20

        Ascii 128 ->
            19

        Ascii 127 ->
            24

        Ascii 126 ->
            17

        Ascii 125 ->
            26

        Ascii 124 ->
            26

        Ascii 123 ->
            16

        Ascii 122 ->
            22

        Ascii 121 ->
            28

        Ascii 120 ->
            20

        Ascii 119 ->
            25

        Ascii 118 ->
            20

        Ascii 117 ->
            17

        Ascii 116 ->
            25

        Ascii 115 ->
            28

        Ascii 114 ->
            24

        Ascii 113 ->
            21

        Ascii 112 ->
            25

        Ascii 111 ->
            18

        Ascii 110 ->
            25

        Ascii 109 ->
            24

        Ascii 108 ->
            3

        Ascii 107 ->
            10

        Ascii 106 ->
            9

        Ascii 105 ->
            16

        Ascii 104 ->
            12

        Ascii 103 ->
            16

        Ascii 102 ->
            22

        Ascii 101 ->
            21

        Ascii 100 ->
            26

        Ascii 99 ->
            34

        Ascii 98 ->
            23

        Ascii 97 ->
            25

        Ascii 96 ->
            25

        Ascii 95 ->
            24

        Ascii 94 ->
            28

        Ascii 93 ->
            28

        Ascii 92 ->
            26

        Ascii 91 ->
            22

        Ascii 90 ->
            32

        Ascii 89 ->
            37

        Ascii 88 ->
            25

        Ascii 87 ->
            28

        Ascii 86 ->
            20

        Ascii 85 ->
            21

        Ascii 84 ->
            30

        Ascii 83 ->
            26

        Ascii 82 ->
            26

        Ascii 81 ->
            30

        Ascii 80 ->
            26

        Ascii 79 ->
            21

        Ascii 78 ->
            29

        Ascii 77 ->
            27

        Ascii 76 ->
            32

        Ascii 75 ->
            15

        Ascii 74 ->
            14

        Ascii 73 ->
            16

        Ascii 72 ->
            14

        Ascii 71 ->
            10

        Ascii 70 ->
            8

        Ascii 69 ->
            23

        Ascii 68 ->
            26

        Ascii 67 ->
            17

        Ascii 66 ->
            26

        Ascii 65 ->
            23

        Ascii 64 ->
            23

        Ascii 63 ->
            22

        Ascii 62 ->
            24

        Ascii 61 ->
            19

        Ascii 60 ->
            24

        Ascii 59 ->
            12

        Ascii 58 ->
            4

        Ascii 57 ->
            7

        Ascii 56 ->
            7

        Ascii 55 ->
            15

        Ascii 54 ->
            14

        Ascii 53 ->
            12

        Ascii 52 ->
            12

        Ascii 51 ->
            9

        Ascii 50 ->
            20

        Ascii 49 ->
            22

        Ascii 48 ->
            25

        Ascii 47 ->
            36

        Ascii 46 ->
            12

        Ascii 45 ->
            13

        Ascii 44 ->
            0

        Ascii 43 ->
            180

        Ascii 42 ->
            153

        Ascii 41 ->
            99

        Ascii 40 ->
            54

        Ascii 39 ->
            14

        Ascii 38 ->
            14

        Ascii 37 ->
            37

        Ascii 36 ->
            46

        Ascii 35 ->
            24

        Ascii 34 ->
            20

        Ascii 33 ->
            21

        Ascii 32 ->
            25

        Ascii 31 ->
            26

        Ascii 30 ->
            28

        Ascii 29 ->
            30

        Ascii 28 ->
            29

        Ascii 27 ->
            50

        Ascii 26 ->
            22

        Ascii 25 ->
            45

        Ascii 24 ->
            33

        Ascii 23 ->
            37

        Ascii 22 ->
            30

        Ascii 21 ->
            30

        Ascii 20 ->
            40

        Ascii 19 ->
            28

        Ascii 18 ->
            37

        Ascii 17 ->
            17

        Ascii 16 ->
            29

        Ascii 15 ->
            21

        Ascii 14 ->
            20

        Ascii 13 ->
            17

        Ascii 12 ->
            14

        Ascii 11 ->
            22

        Ascii 10 ->
            26

        Ascii 9 ->
            29

        Ascii 8 ->
            29

        Ascii 7 ->
            40

        Ascii 6 ->
            41

        Ascii 5 ->
            20

        Ascii 4 ->
            26

        Ascii 3 ->
            41

        Ascii 2 ->
            29

        Ascii 1 ->
            24

        Ascii 0 ->
            21

        _ ->
            0
