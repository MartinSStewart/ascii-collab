module Image.Internal.Decode exposing (andMap, array, foldl, listR, unsignedInt24)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Endianness)
import Bytes.Decode as D exposing (Decoder, Step(..))


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap argument function =
    D.map2 (<|) function argument


listR : Int -> Decoder a -> Decoder (List a)
listR count decoder =
    D.loop ( count, [] ) (listStep decoder)


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        D.succeed (Done xs)

    else
        D.map (\x -> Loop ( n - 1, x :: xs )) decoder


array : Int -> Decoder a -> Decoder (Array a)
array count decoder =
    D.loop ( count, Array.empty ) (arrayStep decoder)


arrayStep : Decoder a -> ( Int, Array a ) -> Decoder (Step ( Int, Array a ) (Array a))
arrayStep decoder ( n, xs ) =
    if n <= 0 then
        D.succeed (Done xs)

    else
        D.map (\x -> Loop ( n - 1, Array.push x xs )) decoder


foldl : Int -> (b -> Decoder b) -> b -> Decoder b
foldl count decoder acc =
    D.loop ( count, acc ) (foldlStep decoder)


foldlStep : (a -> Decoder b) -> ( Int, a ) -> Decoder (Step ( Int, b ) a)
foldlStep decoder ( n, acc ) =
    if n <= 0 then
        D.succeed (Done acc)

    else
        D.map (\x -> Loop ( n - 1, x )) (decoder acc)


unsignedInt24 : Endianness -> Decoder Int
unsignedInt24 endianness =
    case endianness of
        Bytes.LE ->
            D.map2 (\b2 b1 -> Bitwise.or (Bitwise.shiftLeftBy 16 b1) b2) (D.unsignedInt16 endianness) D.unsignedInt8

        Bytes.BE ->
            D.map2 (\b1 b2 -> Bitwise.or (Bitwise.shiftLeftBy 8 b1) b2 {- |> Bitwise.shiftRightZfBy 0 -}) (D.unsignedInt16 endianness) D.unsignedInt8
