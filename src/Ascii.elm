module Ascii exposing (Ascii(..), asciiChars, asciis, charsPerRow, codec, default, fromChar, fromInt, image, intensity, size, textureData, textureHeight, texturePosition, texturePositionInt, textureWidth, toChar, toInt)

import Array exposing (Array)
import Base64
import Bounds exposing (Bounds)
import Dict exposing (Dict)
import Helper exposing (Coord)
import Image exposing (Image)
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import Math.Vector2 exposing (Vec2)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Serialize


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


image : Array (Array Image.Pixel)
image =
    String.dropLeft (String.length "data:image/png;base64,") textureData
        |> Base64.toBytes
        |> Maybe.andThen Image.decode
        |> Maybe.withDefault (Image.fromList2d [])
        |> Image.toArray2d
        |> Array.map
            (Array.map
                (\pixel ->
                    if pixel + 1 == 0 then
                        255

                    else
                        -1
                )
            )


textureWidth : Quantity number Pixels
textureWidth =
    Pixels.pixels 256


textureHeight : Quantity number Pixels
textureHeight =
    Pixels.pixels 512


type Ascii
    = Ascii Int


codec : Serialize.Codec e Ascii
codec =
    Serialize.map Ascii (\(Ascii a) -> a) Serialize.byte


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
            (modBy charsPerRow ascii_ |> (*) w |> toFloat |> (\a -> a / Pixels.inPixels textureWidth))
            (ascii_ // charsPerRow |> (*) h |> toFloat |> (\a -> a / Pixels.inPixels textureHeight))
    , bottomRight =
        Math.Vector2.vec2
            (modBy charsPerRow ascii_ |> (+) 1 |> (*) w |> toFloat |> (\a -> a / Pixels.inPixels textureWidth))
            (ascii_ // charsPerRow |> (+) 1 |> (*) h |> toFloat |> (\a -> a / Pixels.inPixels textureHeight))
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
