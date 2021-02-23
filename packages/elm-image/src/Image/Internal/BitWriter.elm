module Image.Internal.BitWriter exposing
    ( BitWriter
    , empty
    , flush
    , run
    , writeBit
    , writeBits
    , writeEncoder
    )

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Encode as Encode exposing (Encoder)


type alias BitWriter =
    { tag : Int, bitsWritten : Int, encoders : List Encoder }


run : BitWriter -> List Encoder
run state =
    List.reverse state.encoders


empty : BitWriter
empty =
    { tag = 0, bitsWritten = 0, encoders = [] }


{-| Write some bits

New bits are placed on the left (in the higher positions)

-}
writeBits : Int -> Int -> BitWriter -> BitWriter
writeBits bitwidth bits state =
    flushIfNeeded (Bitwise.or state.tag (Bitwise.shiftLeftBy state.bitsWritten bits)) (state.bitsWritten + bitwidth) state.encoders


writeBit : Bool -> BitWriter -> BitWriter
writeBit b =
    case b of
        False ->
            writeBits 1 0

        True ->
            writeBits 1 1


{-| unsafely put an encoder into the list of encoders

This is only safe if the reader was just flushed, otherwise some of the data might be in the tag giving incorrect results.

-}
writeEncoder : Encoder -> BitWriter -> BitWriter
writeEncoder encoder state =
    { tag = state.tag, bitsWritten = state.bitsWritten, encoders = encoder :: state.encoders }


{-| flush the lower 16 bits
-}
flushIfNeeded : Int -> Int -> List Encoder -> BitWriter
flushIfNeeded tag bitsWritten encoders =
    if bitsWritten >= 16 then
        { encoders = Encode.unsignedInt16 LE tag :: encoders
        , bitsWritten = bitsWritten - 16
        , tag = Bitwise.shiftRightBy 16 tag
        }

    else
        { tag = tag, bitsWritten = bitsWritten, encoders = encoders }


{-| Force flush all written bits
-}
flush : BitWriter -> BitWriter
flush state =
    flushLoop state.tag state.bitsWritten state.encoders


flushLoop : Int -> Int -> List Encoder -> BitWriter
flushLoop tag bitsWritten encoders =
    if bitsWritten > 0 then
        flushLoop (Bitwise.shiftRightBy 8 tag) (max 0 (bitsWritten - 8)) (Encode.unsignedInt8 tag :: encoders)

    else
        { tag = tag, bitsWritten = bitsWritten, encoders = encoders }
