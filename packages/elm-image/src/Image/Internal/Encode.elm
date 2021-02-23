module Image.Internal.Encode exposing (unsignedInt24)

import Bitwise exposing (and, shiftRightBy)
import Bytes exposing (Endianness)
import Bytes.Encode as Encode exposing (Encoder)


unsignedInt24 : Endianness -> Int -> Encoder
unsignedInt24 endian num =
    if endian == Bytes.LE then
        Encode.sequence
            [ and num 0xFF |> Encode.unsignedInt8
            , shiftRightBy 8 (and num 0xFF00) |> Encode.unsignedInt8
            , shiftRightBy 16 (and num 0x00FF0000) |> Encode.unsignedInt8
            ]

    else
        Encode.sequence
            [ shiftRightBy 16 (and num 0x00FF0000) |> Encode.unsignedInt8
            , shiftRightBy 8 (and num 0xFF00) |> Encode.unsignedInt8
            , and num 0xFF |> Encode.unsignedInt8
            ]
