module Image.Internal.BMP exposing (decode, encode)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D exposing (Decoder, Step(..))
import Bytes.Encode as E exposing (Encoder, unsignedInt16, unsignedInt32, unsignedInt8)
import Image.Internal.Decode as D
import Image.Internal.Encode exposing (unsignedInt24)
import Image.Internal.ImageData as ImageData exposing (EncodeOptions, Image(..), Order(..))
import Image.Internal.Meta as MetaData exposing (BmpBitsPerPixel(..), BmpHeader)


decode : Bytes -> Maybe Image
decode bytes =
    D.decode (decoder bytes) bytes


decoder : Bytes -> Decoder Image
decoder bytes =
    D.string 2
        |> D.andThen
            (\bm ->
                if bm == "BM" then
                    decodeInfo
                        |> D.map (\bmInfo -> Lazy (MetaData.Bmp bmInfo) (\info -> D.decode (imageDecoder bmInfo) bytes |> Maybe.withDefault (List info [])))

                else
                    D.fail
            )


imageDecoder : BmpHeader -> Decoder Image
imageDecoder info =
    case info.bitsPerPixel of
        BmpBitsPerPixel32 ->
            decode32 info

        BmpBitsPerPixel24 ->
            decode24 info

        BmpBitsPerPixel16 ->
            decode16 info

        BmpBitsPerPixel8 ->
            decode8 info


decodeInfo : Decoder BmpHeader
decodeInfo =
    D.succeed
        (\fileSize _ pixelStart dibHeader width height color_planes bitsPerPixel compression dataSize ->
            { fileSize = fileSize
            , pixelStart = pixelStart
            , dibHeader = dibHeader
            , width = width
            , height = height
            , color_planes = color_planes
            , bitsPerPixel = bitsPerPixel
            , compression = compression
            , dataSize = dataSize
            }
        )
        |> D.andMap (D.unsignedInt32 LE)
        |> D.andMap (D.unsignedInt32 LE)
        |> D.andMap (D.unsignedInt32 LE)
        |> D.andMap (D.unsignedInt32 LE)
        |> D.andMap (D.unsignedInt32 LE)
        |> D.andMap (D.unsignedInt32 LE)
        |> D.andMap (D.unsignedInt16 LE)
        |> D.andMap bitsPerPixelDecode
        |> D.andMap (D.unsignedInt32 LE)
        |> D.andMap (D.unsignedInt32 LE)


bitsPerPixelDecode : Decoder BmpBitsPerPixel
bitsPerPixelDecode =
    D.unsignedInt16 LE
        |> D.andThen
            (\a ->
                case a of
                    8 ->
                        D.succeed BmpBitsPerPixel8

                    16 ->
                        D.succeed BmpBitsPerPixel16

                    24 ->
                        D.succeed BmpBitsPerPixel24

                    32 ->
                        D.succeed BmpBitsPerPixel32

                    _ ->
                        D.fail
            )


encode : Image -> Bytes
encode image =
    encodeFolder (encodeConfig image) (ImageData.toList2d image) 0 0 []
        |> E.sequence
        |> E.encode


encodeConfig : Image -> EncodeConfig
encodeConfig image =
    let
        bytesPerPixel =
            ImageData.getInfo image |> ImageData.bytesPerPixel

        width =
            ImageData.width image

        -- Width is rounded up to a multiple of 4 (word - 32bit)
        extraBytes =
            Bitwise.and (4 - Bitwise.and (width * bytesPerPixel) bytesPerPixel) bytesPerPixel
                |> remainderBy 4

        encoder =
            intToBytes bytesPerPixel
    in
    { bytesPerPixel = bytesPerPixel
    , width = width
    , orderRight = True
    , orderUp = False
    , header32 = bytesPerPixel == 4
    , extraBytes = extraBytes
    , encoder = encoder
    , defaultColors = List.repeat width (encoder 0x00)
    }


type alias EncodeConfig =
    { bytesPerPixel : Int
    , width : Int
    , orderRight : Bool
    , orderUp : Bool
    , header32 : Bool
    , extraBytes : Int
    , encoder : Int -> Encoder
    , defaultColors : List Encoder
    }


{-| Encode a bmp image

Encoding is not entirely straightforward.
We must encode a rectangular image where the width must also be aligned (i.e. padded in some cases).
That means handling incomplete rows and adding padding bytes as needed.

The chosen image representation is `List (List a)`, which makes sense from a memory perspective (Array is slow), but means that efficient traversal
with a recursive function reverses the elements.

This function is written in such a way that as much can be re-used between iterations as possible.
This really matters! any prevented allocation quickly gives big speed increases.

-}
encodeFolder : EncodeConfig -> List (List Int) -> Int -> Int -> List Encoder -> List Encoder
encodeFolder ({ width, extraBytes } as config) remaining height totalBytes acc =
    case remaining of
        row :: rest ->
            let
                -- determine row length and encode its elements
                -- flips the pixel order, so we likely must reverse later
                initial =
                    encodeRow config.encoder row 0 []

                -- if this row has fewer pixels than the width
                -- pad it with the default color
                encoded =
                    let
                        padding =
                            width - initial.width
                    in
                    case padding of
                        0 ->
                            initial.row

                        _ ->
                            List.take padding config.defaultColors ++ initial.row

                withRow =
                    case extraBytes of
                        0 ->
                            -- if the order is left-to-right, `encodeRow` has flipped the order, so reverse
                            if config.orderRight then
                                E.sequence (List.reverse encoded) :: acc

                            else
                                E.sequence encoded :: acc

                        _ ->
                            -- if the order is left-to-right, `encodeRow` has flipped the order, so reverse
                            if config.orderRight then
                                E.sequence (List.reverse (addRowPadding extraBytes encoded)) :: acc

                            else
                                E.sequence (encoded ++ addRowPadding extraBytes []) :: acc
            in
            encodeFolder config rest (height + 1) (totalBytes + width * config.bytesPerPixel + extraBytes) withRow

        [] ->
            let
                body =
                    if config.orderUp then
                        List.reverse acc

                    else
                        acc
            in
            if config.header32 then
                header32 width height totalBytes body

            else
                header16_24 (8 * config.bytesPerPixel) width height totalBytes body


{-| encode a row (usually `b ~ Encoder`)

This is really like a `List.map` that reverses the input. Unfortunate, but it is the fastest way.

-}
encodeRow : (a -> b) -> List a -> Int -> List b -> { width : Int, row : List b }
encodeRow f items i acc =
    case items of
        px :: rest ->
            encodeRow f rest (i + 1) (f px :: acc)

        [] ->
            { width = i, row = acc }


addRowPadding : Int -> List Encoder -> List Encoder
addRowPadding n acc =
    case n of
        1 ->
            E.unsignedInt8 0 :: acc

        2 ->
            E.unsignedInt16 LE 0 :: acc

        3 ->
            E.unsignedInt16 LE 0 :: E.unsignedInt8 0 :: acc

        _ ->
            E.unsignedInt32 LE 0 :: acc


intToBytes : Int -> (Int -> Encoder)
intToBytes bpp =
    case bpp of
        1 ->
            unsignedInt8

        2 ->
            unsignedInt16 Bytes.LE

        3 ->
            unsignedInt24 Bytes.LE

        _ ->
            unsignedInt32 Bytes.LE


header16_24 : Int -> Int -> Int -> Int -> List Encoder -> List Encoder
header16_24 bitsPerPixel w h dataSize accum =
    {- BMP Header -}
    -- "BM" -|- ID field ( 42h, 4Dh )
    --   [ 0x42, 0x4D ] |> List.map unsignedInt8
    unsignedInt16 BE 0x424D
        --   70 bytes (54+16) -|- Size of the BMP file
        -- , [ 0x46, 0x00, 0x00, 0x00 ] |> List.map unsignedInt8
        :: unsignedInt32 LE (54 + dataSize)
        -- -- Unused -|- Application specific
        -- , [ 0x00, 0x00 ] |> List.map
        -- -- Unused -|- Application specific
        -- , [ 0x00, 0x00 ] |> List.map unsignedInt8
        :: unsignedInt32 LE 0
        -- 54 bytes (14+40) -|- Offset where the pixel array (bitmap data) can be found
        -- , [ 0x36, 0x00, 0x00, 0x00 ] |> List.map unsignedInt8
        :: unsignedInt32 LE (14 + 40)
        {- DIB Header -}
        --40 bytes -|- Number of bytes in the DIB header (from this point)
        -- , [ 0x28, 0x00, 0x00, 0x00 ] |> List.map unsignedInt8
        :: unsignedInt32 LE 40
        -- 2 pixels (left to right order) -|- Width of the bitmap in pixels
        -- , [ 0x02, 0x00, 0x00, 0x00 ] |> List.map unsignedInt8
        :: unsignedInt32 LE w
        -- 2 pixels (bottom to top order) -|- Height of the bitmap in pixels. Positive for bottom to top pixel order.
        -- , [ 0x02, 0x00, 0x00, 0x00 ] |> List.map unsignedInt8
        :: unsignedInt32 LE h
        --1 plane -|-  Number of color planes being used
        -- , [ 0x01, 0x00 ] |> List.map unsignedInt8
        :: unsignedInt16 LE 1
        -- 24 bits -|- Number of bits per pixel
        -- , [ 0x18, 0x00 ] |> List.map unsignedInt8
        :: unsignedInt16 LE bitsPerPixel
        -- 0 -|- BI_RGB, no pixel array compression used
        -- , [ 0x00, 0x00, 0x00, 0x00 ] |> List.map unsignedInt8
        :: unsignedInt32 LE 0
        -- 16 bytes -|- Size of the raw bitmap data (including padding)
        -- , [ 0x10, 0x00, 0x00, 0x00 ] |> List.map unsignedInt8
        :: unsignedInt32 LE dataSize
        -- 2835 pixels/metre horizontal  | Print resolution of the image, 72 DPI × 39.3701 inches per metre yields 2834.6472
        -- , [ 0x13, 0x0B, 0x00, 0x00 ] |> List.map unsignedInt8
        :: unsignedInt32 LE 2835
        -- 2835 pixels/metre vertical
        -- , [ 0x13, 0x0B, 0x00, 0x00 ] |> List.map unsignedInt8
        :: unsignedInt32 LE 2835
        -- 0 colors -|- Number of colors in the palette
        -- , [ 0x00, 0x00, 0x00, 0x00 ] |> List.map unsignedInt8
        :: unsignedInt32 LE 0
        -- 0 important colors -|- 0 means all colors are important
        -- , [ 0x00, 0x00, 0x00, 0x00 ] |> List.map unsignedInt8
        :: unsignedInt32 LE 0
        :: accum


header32 : Int -> Int -> Int -> List Encoder -> List Encoder
header32 w h dataSize accum =
    {- BMP Header -}
    unsignedInt16 BE 0x424D
        -- "BM"    ID field (42h, 4Dh)
        :: unsignedInt32 LE (122 + dataSize)
        --  Size of the BMP file
        :: unsignedInt32 LE 0
        --Unused - Application specific
        :: unsignedInt32 LE 122
        --122 bytes (14+108) Offset where the pixel array (bitmap data) can be found
        {- DIB Header -}
        :: unsignedInt32 LE 108
        -- 108 bytes    Number of bytes in the DIB header (from this point)
        :: unsignedInt32 LE w
        -- Width of the bitmap in pixels
        :: unsignedInt32 LE h
        -- Height of the bitmap in pixels
        :: unsignedInt16 LE 1
        --  Number of color planes being used
        :: unsignedInt16 LE 32
        -- Number of bits per pixel
        :: unsignedInt32 LE 3
        -- BI_BITFIELDS:: no pixel array compression used
        :: unsignedInt32 LE dataSize
        -- Size of the raw bitmap data (including padding)
        :: E.bytes staticHeaderPart
        :: accum


{-| This part of the header is always the same, it's pre-calculated on startup.
-}
staticHeaderPart : Bytes
staticHeaderPart =
    (E.sequence >> E.encode) <|
        [ -- 2835 pixels/metre horizontal
          unsignedInt32 LE 2835

        -- 2835 pixels/metre vertical
        , unsignedInt32 LE 2835

        -- Number of colors in the palette
        , unsignedInt32 LE 0

        --important colors (0 means all colors are important)
        , unsignedInt32 LE 0

        --00FF0000 in big-endian Red channel bit mask (valid because BI_BITFIELDS is specified)
        , unsignedInt32 LE 0xFF000000

        --0000FF00 in big-endian    Green channel bit mask (valid because BI_BITFIELDS is specified)
        , unsignedInt32 LE 0x00FF0000

        --000000FF in big-endian    Blue channel bit mask (valid because BI_BITFIELDS is specified)
        , unsignedInt32 LE 0xFF00

        --FF000000 in big-endian    Alpha channel bit mask
        , unsignedInt32 LE 0xFF

        --   little-endian "Win "    LCS_WINDOWS_COLOR_SPACE
        --CIEXYZTRIPLE Color Space endpoints    Unused for LCS "Win " or "sRGB"
        , unsignedInt32 LE 0x206E6957
        , unsignedInt32 LE 0
        , unsignedInt32 LE 0
        , unsignedInt32 LE 0
        , unsignedInt32 LE 0
        , unsignedInt32 LE 0
        , unsignedInt32 LE 0
        , unsignedInt32 LE 0
        , unsignedInt32 LE 0
        , unsignedInt32 LE 0

        -----------
        --0 Red Gamma    Unused for LCS "Win " or "sRGB"
        , unsignedInt32 LE 0

        --0 Green Gamma    Unused for LCS "Win " or "sRGB"
        , unsignedInt32 LE 0

        --0 Blue Gamma    Unused for LCS "Win " or "sRGB"
        , unsignedInt32 LE 0
        ]


decode32 : BmpHeader -> Decoder Image
decode32 info =
    D.bytes info.pixelStart
        |> D.andThen (\_ -> D.listR info.height (D.listR info.width (D.unsignedInt32 LE) |> D.map List.reverse))
        |> D.map (List2d (MetaData.Bmp info))


decode24 : BmpHeader -> Decoder Image
decode24 info =
    D.bytes info.pixelStart
        |> D.andThen (\_ -> D.listR info.height (D.listR info.width (D.unsignedInt24 LE) |> D.map List.reverse))
        |> D.map (List2d (MetaData.Bmp info))


decode16 : BmpHeader -> Decoder Image
decode16 info =
    D.bytes info.pixelStart
        |> D.andThen (\_ -> D.listR info.height (D.listR info.width (D.unsignedInt16 LE) |> D.map List.reverse))
        |> D.map (List2d (MetaData.Bmp info))


decode8 : BmpHeader -> Decoder Image
decode8 info =
    D.bytes info.pixelStart
        |> D.andThen (\_ -> D.listR info.height (D.listR info.width D.unsignedInt8 |> D.map List.reverse))
        |> D.map (List2d (MetaData.Bmp info))
