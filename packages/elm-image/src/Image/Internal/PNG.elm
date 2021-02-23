module Image.Internal.PNG exposing (decode, encode)

--https://www.w3.org/TR/PNG-Structure.html
--https://www.w3.org/TR/PNG/

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D exposing (Decoder, Step(..))
import Bytes.Encode as E exposing (Encoder)
import Dict
import Flate exposing (crc32, deflateZlib, inflateZlib)
import Image.Internal.Array2D as Array2d exposing (Array2D)
import Image.Internal.Decode as D
import Image.Internal.ImageData as ImageData exposing (EncodeOptions, Image(..), Order(..), PixelFormat(..), defaultOptions)
import Image.Internal.Meta as Metadata exposing (BitDepth1_2_4_8(..), BitDepth1_2_4_8_16(..), BitDepth8_16(..), PngColor(..), PngHeader)


type alias PngEncodeOptions =
    { color : PngColor }


encode : Image -> Bytes
encode imgData =
    let
        opt =
            defaultOptions

        arr =
            ImageData.toArray2d imgData

        height =
            Array.length arr

        width =
            Array.get 0 arr |> Maybe.map Array.length |> Maybe.withDefault 0

        chunkIHDR =
            encodeChunk 1229472850 (encodeIHDR width height opt |> E.encode)

        chunkIDAT =
            encodeChunk 1229209940 (encodeIDAT opt arr |> E.encode)

        chunkIEND =
            encodeChunk 1229278788 (E.sequence [] |> E.encode)
    in
    E.sequence
        [ encodeSignature
        , chunkIHDR
        , chunkIDAT
        , chunkIEND
        ]
        |> E.encode


encodeSignature =
    E.sequence
        [ E.unsignedInt8 137
        , E.unsignedInt8 80
        , E.unsignedInt8 78
        , E.unsignedInt8 71
        , E.unsignedInt8 13
        , E.unsignedInt8 10
        , E.unsignedInt8 26
        , E.unsignedInt8 10
        ]


encodeChunk : Int -> Bytes -> Encoder
encodeChunk kind data =
    let
        length =
            Bytes.width data

        kindAndData =
            E.encode (E.sequence [ E.unsignedInt32 BE kind, E.bytes data ])
    in
    E.sequence
        [ E.unsignedInt32 BE length
        , E.bytes kindAndData
        , E.unsignedInt32 BE (crc32 kindAndData)
        ]


encodeIDAT : { a | order : Order } -> Array (Array Int) -> Encoder
encodeIDAT { order } arr =
    let
        scanLineFilter =
            E.unsignedInt8 1

        ( fold1, fold2 ) =
            case order of
                RightDown ->
                    ( Array.foldl, Array.foldl )

                RightUp ->
                    ( Array.foldr, Array.foldl )

                LeftDown ->
                    ( Array.foldl, Array.foldr )

                LeftUp ->
                    ( Array.foldr, Array.foldr )
    in
    fold1
        (\sArr acc ->
            fold2
                (\px ( prev, acc2 ) ->
                    let
                        packed =
                            encodePixel32 px prev
                    in
                    ( px, packed :: acc2 )
                )
                ( 0, [ scanLineFilter ] )
                sArr
                |> (\( _, line ) -> line :: acc)
        )
        []
        arr
        |> (List.reverse
                >> List.concatMap List.reverse
                >> E.sequence
                >> E.encode
                >> deflateZlib
                >> E.bytes
           )


encodePixel32 : Int -> Int -> Encoder
encodePixel32 px prev =
    let
        r =
            px |> Bitwise.shiftRightZfBy 24

        a =
            px |> Bitwise.and 0xFF

        b =
            px |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF

        g =
            px |> Bitwise.shiftRightBy 16 |> Bitwise.and 0xFF

        prevR =
            prev
                |> Bitwise.shiftRightZfBy 24

        prevG =
            prev
                |> Bitwise.shiftRightBy 16
                |> Bitwise.and 0xFF

        prevB =
            prev |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF

        prevA =
            prev |> Bitwise.and 0xFF
    in
    packIntoInt32 (r - prevR) (g - prevG) (b - prevB) (a - prevA)
        |> E.unsignedInt32 BE


encodeIHDR : Int -> Int -> { a | format : PixelFormat } -> Encoder
encodeIHDR width height { format } =
    let
        {- 0       1,2,4,8,16  Each pixel is a grayscale sample.

           2       8,16        Each pixel is an R,G,B triple.

           3       1,2,4,8     Each pixel is a palette index;
                               a PLTE chunk must appear.

           4       8,16        Each pixel is a grayscale sample,
                               followed by an alpha sample.

           6       8,16        Each pixel is an R,G,B triple,
                               followed by an alpha sample.
        -}
        ( depth, color ) =
            case format of
                RGBA ->
                    ( 8, 6 )

                RGB ->
                    ( 8, 2 )

                LUMINANCE_ALPHA ->
                    ( 16, 0 )

                ALPHA ->
                    ( 8, 0 )

        --0 (no interlace) or 1 (Adam7 interlace)
        interlace =
            0
    in
    --       Width:              4 bytes
    --       Height:             4 bytes
    --       Bit depth:          1 byte
    --       Color type:         1 byte
    --       Compression method: 1 byte
    --       Filter method:      1 byte
    --       Interlace method:   1 byte
    E.sequence
        [ E.unsignedInt32 BE width
        , E.unsignedInt32 BE height
        , E.unsignedInt8 depth
        , E.unsignedInt8 color
        , E.unsignedInt8 0
        , E.unsignedInt8 0
        , E.unsignedInt8 interlace
        ]


{-| DECODE STARTS HERE
-}
decode : Bytes -> Maybe Image
decode bytes =
    D.decode (mainDecoder <| Bytes.width bytes - 8) bytes


mainDecoder : Int -> Decoder Image
mainDecoder chunksLength =
    D.listR 8 D.unsignedInt8
        |> D.andThen
            (\signature ->
                if signature == [ 10, 26, 10, 13, 71, 78, 80, 137 ] then
                    D.succeed ()

                else
                    D.fail
            )
        |> D.andThen (\_ -> chunkLoopR chunksLength decodeChunk)
        |> D.andThen
            (\maybeImage ->
                case maybeImage of
                    Just image ->
                        case image.data of
                            ImageData data ->
                                D.succeed data

                            _ ->
                                D.fail

                    _ ->
                        D.fail
            )


type alias PNG =
    { header : PngHeader
    , data : Pixels

    {- There shall not be more than one PLTE chunk. -}
    , palette : Palette
    }


type alias Palette =
    Array Int


type Pixels
    = None
    | Collecting Encoder
    | ImageData Image


decodeChunk : Maybe PNG -> Decoder ( Int, Maybe PNG )
decodeChunk acc =
    D.unsignedInt32 BE
        |> D.andThen
            (\length ->
                D.map2
                    (\kindAndData crc ->
                        --                        if crc32 kindAndData == crc then
                        D.decode (D.andThen (chunkCollector length acc) (D.unsignedInt32 BE)) kindAndData
                            |> Maybe.map D.succeed
                            |> Maybe.withDefault D.fail
                            -- `length` 4-byte unsigned integer giving the number of bytes in the chunk's data field.
                            -- The length counts only the data field, not itself, the chunk type code, or the CRC.
                            |> D.map (Tuple.pair (length + 12))
                    )
                    (D.bytes (length + 4))
                    (D.unsignedInt32 BE)
            )
        |> D.andThen identity


chunkCollector : Int -> Maybe PNG -> Int -> Decoder (Maybe PNG)
chunkCollector length acc kind =
    case ( acc, kind, length ) of
        ( Nothing, 1229472850, 13 ) ->
            decodeIHDR
                |> D.map Just

        ( Just image, 1347179589, _ ) ->
            decodePLTE image length
                |> D.map Just

        ( Just image, 1951551059, _ ) ->
            decode_tRNS image length
                |> D.map Just

        ( Just image, 1229209940, _ ) ->
            decodeIDAT image length
                |> D.map Just

        ( Just image, 1229278788, _ ) ->
            decodeIEND image length
                |> D.map Just

        ( Just image, _, _ ) ->
            decodeUnknown kind image length
                |> D.map Just

        _ ->
            D.fail


{-|

    Width:              4 bytes
    Height:             4 bytes
    Bit depth:          1 byte
    Color type:         1 byte
    Compression method: 1 byte
    Filter method:      1 byte
    Interlace method:   1 byte

-}
decodeIHDR : Decoder PNG
decodeIHDR =
    D.succeed
        (\width height color _ _ adam7 ->
            { header =
                { width = width
                , height = height
                , color = color
                , adam7 = adam7
                , chunks = Dict.empty
                }
            , data = None
            , palette = Array.empty
            }
        )
        |> D.andMap (D.unsignedInt32 BE)
        |> D.andMap (D.unsignedInt32 BE)
        |> D.andMap decodeIHDRColor
        |> D.andMap D.unsignedInt8
        |> D.andMap D.unsignedInt8
        |> D.andMap decodeInterlace


decodeInterlace : Decoder Bool
decodeInterlace =
    D.unsignedInt8
        |> D.andThen
            (\interlace ->
                case interlace of
                    0 ->
                        D.succeed False

                    1 ->
                        D.succeed True

                    _ ->
                        D.fail
            )


decodePLTE : PNG -> Int -> Decoder PNG
decodePLTE image length =
    D.array (length // 3) (D.unsignedInt24 BE)
        |> D.map (\palette -> { image | palette = palette })


decode_tRNS : PNG -> Int -> Decoder PNG
decode_tRNS image length =
    --  https://www.w3.org/TR/PNG/#11tRNS
    case image.header.color of
        IndexedColour _ ->
            D.foldl length
                (\( i, acc ) ->
                    D.unsignedInt8
                        |> D.map
                            (\alpha ->
                                ( i + 1, arrayExtraUpdate i (Bitwise.shiftLeftBy 8 >> Bitwise.or alpha >> Bitwise.shiftRightZfBy 0) acc )
                            )
                )
                ( 0, image.palette )
                |> D.map (\( _, palette ) -> { image | palette = palette })

        _ ->
            D.fail


decodeIDAT : PNG -> Int -> Decoder PNG
decodeIDAT image length =
    D.bytes length
        |> D.map
            (\b ->
                case image.data of
                    Collecting e ->
                        { image | data = Collecting (E.sequence [ e, E.bytes b ]) }

                    None ->
                        { image | data = Collecting (E.bytes b) }

                    ImageData _ ->
                        image
            )


decodeIEND : PNG -> Int -> Decoder PNG
decodeIEND ({ header, palette } as image) length =
    D.bytes length
        |> D.andThen
            (\_ ->
                case image.data of
                    Collecting e ->
                        let
                            bytes =
                                E.encode e

                            output =
                                Lazy (Metadata.Png header)
                                    (\info ->
                                        D.decode (imageDecoder header palette (Bytes.width bytes)) bytes |> Maybe.withDefault (List info [])
                                    )
                        in
                        D.succeed { image | data = ImageData output }

                    _ ->
                        D.fail
            )


imageDecoder : PngHeader -> Palette -> Int -> Decoder Image
imageDecoder ({ width, height, color } as header) palette total =
    D.bytes total
        |> D.andThen
            (inflateZlib
                >> Maybe.andThen (D.decode (dataDecode header palette))
                >> Maybe.map D.succeed
                >> Maybe.withDefault D.fail
            )


dataDecode : PngHeader -> Palette -> Decoder Image
dataDecode ({ width, height, color } as header) palette =
    case color of
        IndexedColour BitDepth1_2_4_8__8 ->
            dataWrapDecode (indexPixel8Decode palette) width height
                |> D.map (Array2d (Metadata.Png header))

        GreyscaleAlpha BitDepth8_16__8 ->
            dataWrapDecode pixel16Decode width height
                |> D.map (Array2d (Metadata.Png header))

        TrueColourAlpha BitDepth8_16__8 ->
            dataWrapDecode pixel32Decode width height
                |> D.map (Array2d (Metadata.Png header))

        _ ->
            D.fail


dataWrapDecode : (Int -> Array2D a -> Decoder (Array2D a)) -> Int -> Int -> Decoder (Array2D a)
dataWrapDecode pxDecode width height =
    D.foldl height (decodeLine pxDecode width) Array.empty


decodeLine : (Int -> Array2D a -> Decoder (Array2D a)) -> Int -> Array2D a -> Decoder (Array2D a)
decodeLine pxDecode width acc =
    D.unsignedInt8 |> D.andThen (\filterType -> D.foldl width (pxDecode filterType) (Array.push Array.empty acc))


indexPixel8Decode : Palette -> Int -> Array2D Int -> Decoder (Array2D Int)
indexPixel8Decode palette filterType acc =
    pixel8DecodePlain filterType acc
        |> D.andThen
            (\i ->
                Array.get i palette
                    |> Maybe.map (\c -> Array2d.push c acc |> D.succeed)
                    |> Maybe.withDefault D.fail
            )



--pixel8Decode : Int -> Array2D Int -> Decoder (Array2D Int)
--pixel8Decode filterType acc =
--    pixel8DecodePlain filterType acc
--        |> D.map (\a -> Array2d.push a acc)


pixel8DecodePlain : Int -> Array2D Int -> Decoder Int
pixel8DecodePlain filterType acc =
    case filterType of
        0 ->
            pixelDecode8None_

        1 ->
            pixelDecode8Sub_ acc

        2 ->
            pixelDecode8Up_ acc

        4 ->
            pixelDecode8Paeth_ acc

        _ ->
            pixelDecode8None_


pixel16Decode : Int -> Array2D Int -> Decoder (Array2D Int)
pixel16Decode filterType acc =
    (case filterType of
        0 ->
            pixelDecode16None_

        1 ->
            pixelDecode16Sub_ acc

        2 ->
            pixelDecode16Up_ acc

        4 ->
            pixelDecode16Paeth_ acc

        _ ->
            pixelDecode16None_
    )
        |> D.map (\a -> Array2d.push a acc)


pixel32Decode : Int -> Array2D Int -> Decoder (Array2D Int)
pixel32Decode filterType acc =
    (case filterType of
        0 ->
            pixelDecode32None_

        1 ->
            pixelDecode32Sub_ acc

        2 ->
            pixelDecode32Up_ acc

        4 ->
            pixelDecode32Paeth_ acc

        _ ->
            pixelDecode32None_
    )
        |> D.map (\a -> Array2d.push a acc)


pixelDecode8None_ =
    D.unsignedInt8


pixelDecode8Sub_ acc =
    D.map
        (\a_ ->
            let
                prev =
                    Array2d.last acc |> Maybe.withDefault 0

                prevA =
                    prev |> Bitwise.and 0xFF

                a =
                    a_ + prevA
            in
            a
        )
        D.unsignedInt8


pixelDecode8Up_ acc =
    D.map
        (\a_ ->
            let
                prev =
                    arrayUp acc
                        |> Maybe.withDefault 0

                prevA =
                    prev |> Bitwise.and 0xFF

                a =
                    a_ + prevA
            in
            a
        )
        D.unsignedInt8


pixelDecode8Paeth_ acc =
    D.map
        (\a_ ->
            let
                ( prevA_, prevB_, prevC_ ) =
                    arrayPaeth acc

                prevA =
                    paeth
                        (Bitwise.and 0xFF prevA_)
                        (Bitwise.and 0xFF prevB_)
                        (Bitwise.and 0xFF prevC_)

                a =
                    a_ + prevA
            in
            a
        )
        D.unsignedInt8


pixelDecode16None_ =
    D.unsignedInt16 BE


pixelDecode16Sub_ acc =
    D.map2
        (\b_ a_ ->
            let
                prev =
                    Array2d.last acc |> Maybe.withDefault 0

                prevA =
                    prev |> Bitwise.and 0xFF

                prevB =
                    prev |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF

                a =
                    a_ + prevA

                b =
                    b_ + prevB
            in
            packIntoInt16 b a
        )
        D.unsignedInt8
        D.unsignedInt8


pixelDecode16Up_ acc =
    D.map2
        (\b_ a_ ->
            let
                prev =
                    arrayUp acc
                        |> Maybe.withDefault 0

                prevA =
                    prev |> Bitwise.and 0xFF

                prevB =
                    prev |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF

                a =
                    a_ + prevA

                b =
                    b_ + prevB
            in
            packIntoInt16 b a
        )
        D.unsignedInt8
        D.unsignedInt8


pixelDecode16Paeth_ acc =
    D.map2
        (\b_ a_ ->
            let
                ( prevA_, prevB_, prevC_ ) =
                    arrayPaeth acc

                prevA =
                    paeth
                        (Bitwise.and 0xFF prevA_)
                        (Bitwise.and 0xFF prevB_)
                        (Bitwise.and 0xFF prevC_)

                prevB =
                    paeth
                        (prevA_ |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF)
                        (prevB_ |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF)
                        (prevC_ |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF)

                a =
                    a_ + prevA

                b =
                    b_ + prevB
            in
            packIntoInt16 b a
        )
        D.unsignedInt8
        D.unsignedInt8


pixelDecode32None_ =
    D.unsignedInt32 BE


pixelDecode32Sub_ acc =
    D.map4
        (\r_ g_ b_ a_ ->
            let
                prev =
                    Array2d.last acc |> Maybe.withDefault 0

                prevA =
                    prev |> Bitwise.and 0xFF

                prevB =
                    prev |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF

                prevG =
                    prev |> Bitwise.shiftRightBy 16 |> Bitwise.and 0xFF

                prevR =
                    prev |> Bitwise.shiftRightZfBy 24

                a =
                    a_ + prevA

                b =
                    b_ + prevB

                g =
                    g_ + prevG

                r =
                    r_ + prevR
            in
            packIntoInt32 r g b a
        )
        D.unsignedInt8
        D.unsignedInt8
        D.unsignedInt8
        D.unsignedInt8


pixelDecode32Up_ acc =
    D.map4
        (\r_ g_ b_ a_ ->
            let
                prev =
                    arrayUp acc
                        |> Maybe.withDefault 0

                prevA =
                    prev |> Bitwise.and 0xFF

                prevB =
                    prev |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF

                prevG =
                    prev |> Bitwise.shiftRightBy 16 |> Bitwise.and 0xFF

                prevR =
                    prev |> Bitwise.shiftRightZfBy 24

                a =
                    a_ + prevA

                b =
                    b_ + prevB

                g =
                    g_ + prevG

                r =
                    r_ + prevR
            in
            packIntoInt32 r g b a
        )
        D.unsignedInt8
        D.unsignedInt8
        D.unsignedInt8
        D.unsignedInt8


pixelDecode32Paeth_ acc =
    D.map4
        (\r_ g_ b_ a_ ->
            let
                ( prevA_, prevB_, prevC_ ) =
                    arrayPaeth acc

                prevA =
                    paeth
                        (Bitwise.and 0xFF prevA_)
                        (Bitwise.and 0xFF prevB_)
                        (Bitwise.and 0xFF prevC_)

                prevB =
                    paeth
                        (prevA_ |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF)
                        (prevB_ |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF)
                        (prevC_ |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF)

                prevG =
                    paeth
                        (prevA_ |> Bitwise.shiftRightBy 16 |> Bitwise.and 0xFF)
                        (prevB_ |> Bitwise.shiftRightBy 16 |> Bitwise.and 0xFF)
                        (prevC_ |> Bitwise.shiftRightBy 16 |> Bitwise.and 0xFF)

                prevR =
                    paeth
                        (prevA_ |> Bitwise.shiftRightBy 24 |> Bitwise.and 0xFF)
                        (prevB_ |> Bitwise.shiftRightBy 24 |> Bitwise.and 0xFF)
                        (prevC_ |> Bitwise.shiftRightBy 24 |> Bitwise.and 0xFF)

                a =
                    a_ + prevA

                b =
                    b_ + prevB

                g =
                    g_ + prevG

                r =
                    r_ + prevR
            in
            packIntoInt32 r g b a
        )
        D.unsignedInt8
        D.unsignedInt8
        D.unsignedInt8
        D.unsignedInt8


arrayUp : Array2D a -> Maybe a
arrayUp arr =
    Array.get (Array.length arr - 2) arr
        |> Maybe.andThen (Array.get (Array2d.lastLength arr))


arrayPaeth : Array (Array number) -> ( number, number, number )
arrayPaeth arr =
    Array.get (Array.length arr - 2) arr
        |> Maybe.map
            (\subArr ->
                let
                    currentIndex =
                        Array2d.lastLength arr
                in
                ( Array2d.last arr |> Maybe.withDefault 0
                , Array.get currentIndex subArr |> Maybe.withDefault 0
                , Array.get (currentIndex - 1) subArr |> Maybe.withDefault 0
                )
            )
        |> Maybe.withDefault ( 0, 0, 0 )


paeth : number -> number -> number -> number
paeth a b c =
    let
        p =
            a + b - c

        pa =
            abs (p - a)

        pb =
            abs (p - b)

        pc =
            abs (p - c)
    in
    if pa <= pb && pa <= pc then
        a

    else if pb <= pc then
        b

    else
        c


decodeUnknown : Int -> PNG -> Int -> Decoder PNG
decodeUnknown kind ({ header } as image) length =
    D.bytes length |> D.map (saveChunk image kind)


saveChunk : PNG -> Int -> Bytes -> PNG
saveChunk ({ header } as image) kind value =
    let
        name =
            E.encode (E.unsignedInt32 BE kind)
                |> D.decode (D.string 4)
                |> Maybe.withDefault ""
    in
    { image | header = { header | chunks = Dict.insert name value header.chunks } }


decodeIHDRColor : Decoder PngColor
decodeIHDRColor =
    D.map2
        (\depth color ->
            case color of
                0 ->
                    D.map Greyscale (fromIntBitDepth1_2_4_8_16 depth)

                2 ->
                    D.map TrueColour (fromIntBitDepth8_16 depth)

                3 ->
                    D.map IndexedColour (fromIntBitDepth1_2_4_8 depth)

                4 ->
                    D.map GreyscaleAlpha (fromIntBitDepth8_16 depth)

                6 ->
                    D.succeed (TrueColourAlpha BitDepth8_16__8)

                _ ->
                    D.fail
        )
        D.unsignedInt8
        D.unsignedInt8
        |> D.andThen identity


fromIntBitDepth1_2_4_8_16 i =
    case i of
        1 ->
            D.succeed BitDepth1_2_4_8_16__1

        2 ->
            D.succeed BitDepth1_2_4_8_16__2

        4 ->
            D.succeed BitDepth1_2_4_8_16__4

        8 ->
            D.succeed BitDepth1_2_4_8_16__8

        6 ->
            D.succeed BitDepth1_2_4_8_16__16

        _ ->
            D.fail


fromIntBitDepth1_2_4_8 i =
    case i of
        1 ->
            D.succeed BitDepth1_2_4_8__1

        2 ->
            D.succeed BitDepth1_2_4_8__2

        4 ->
            D.succeed BitDepth1_2_4_8__4

        8 ->
            D.succeed BitDepth1_2_4_8__8

        _ ->
            D.fail


fromIntBitDepth8_16 i =
    case i of
        8 ->
            D.succeed BitDepth8_16__8

        16 ->
            D.succeed BitDepth8_16__16

        _ ->
            D.fail


chunkLoopR : number -> (Maybe a -> Decoder ( number, Maybe a )) -> Decoder (Maybe a)
chunkLoopR length decoder =
    D.loop ( length, Nothing ) (listStep decoder)


listStep : (b -> Decoder ( number, a )) -> ( number, b ) -> Decoder (Step ( number, a ) a)
listStep decoder_ ( length_, acc ) =
    decoder_ acc
        |> D.map
            (\( bytesTaken, newAcc ) ->
                if length_ - bytesTaken > 0 then
                    Loop ( length_ - bytesTaken, newAcc )

                else
                    Done newAcc
            )


packIntoInt32 : Int -> Int -> Int -> Int -> Int
packIntoInt32 r g b a =
    Bitwise.or
        (Bitwise.or
            (Bitwise.shiftLeftBy 24 (Bitwise.and 0xFF r))
            (Bitwise.shiftLeftBy 16 (Bitwise.and 0xFF g))
        )
        (Bitwise.or
            (Bitwise.shiftLeftBy 8 (Bitwise.and 0xFF b))
            (Bitwise.and 0xFF a)
        )



--        |> Bitwise.shiftRightZfBy 0
--        |> Bitwise.and 0xFFFFFFFF


packIntoInt16 : Int -> Int -> Int
packIntoInt16 b a =
    Bitwise.or
        (Bitwise.shiftLeftBy 8 (Bitwise.and 0xFF b))
        (Bitwise.and 0xFF a)


arrayExtraUpdate : Int -> (a -> a) -> Array a -> Array a
arrayExtraUpdate n f a =
    let
        element =
            Array.get n a
    in
    case element of
        Nothing ->
            a

        Just element_ ->
            Array.set n (f element_) a
