module Ascii exposing (Ascii, CodecError(..), ascii, charsPerRow, codec, default, fromChar, size, textureData, texturePosition, toChar)

import Dict exposing (Dict)
import List.Extra as List
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


type CodecError
    = InvalidAsciiValue


codec : Serialize.Codec CodecError Ascii
codec =
    Serialize.byte |> Serialize.mapValid (ascii >> Result.fromMaybe InvalidAsciiValue) (\(Ascii a) -> a)


ascii : Int -> Maybe Ascii
ascii value =
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
