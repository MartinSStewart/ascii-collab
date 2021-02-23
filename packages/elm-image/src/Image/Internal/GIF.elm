module Image.Internal.GIF exposing (decode, encode)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E
import Image.Internal.Decode as D
import Image.Internal.ImageData as ImageData exposing (Image(..), Order(..), PixelFormat(..))
import Image.Internal.Lzw as Lzw
import Image.Internal.Meta exposing (Header(..))



--https://www.w3.org/Graphics/GIF/spec-gif87.txt
--https://www.matthewflickinger.com/lab/whatsinagif/index.html


type alias GifImage =
    { header :
        { width : Int
        , height : Int
        }
    , palette : Array Int
    }


decode : Bytes -> Maybe Image
decode bytes =
    D.decode (mainDecoder <| Bytes.width bytes) bytes


encode : Image -> Bytes
encode image =
    E.encode (E.sequence [])


mainDecoder : Int -> D.Decoder Image
mainDecoder size =
    decodeSignature
        |> D.andThen (\_ -> decodeInfo)
        |> D.andThen
            (\info ->
                D.bytes (size - 13)
                    |> D.map
                        (\rest ->
                            Lazy
                                (Gif
                                    { width = info.width
                                    , height = info.height
                                    }
                                )
                                (\header ->
                                    D.decode (decodeLazyImage info) rest
                                        |> Maybe.withDefault []
                                        |> ImageData.List header
                                )
                        )
            )


decodeLazyImage : GifInfo -> D.Decoder (List Int)
decodeLazyImage info =
    decodePalette info.bits
        |> D.map
            (\palette ->
                { width = info.width
                , height = info.height
                , m = info.m
                , cr = info.cr
                , bits = info.bits
                , background = info.background
                , palette = palette
                }
            )
        |> D.andThen (\info_ -> decodeFrames info_ ( [], Array.empty ) |> D.map (\( ext, dsc ) -> ( info_, ext, dsc )))
        |> D.andThen
            (\( { background, palette }, ext, dsc ) ->
                case Array.get 0 dsc of
                    Just { data } ->
                        data
                            |> List.map (\i -> Array.get i palette |> Maybe.withDefault 0)
                            |> D.succeed

                    Nothing ->
                        D.fail
            )


decodeSignature : D.Decoder ()
decodeSignature =
    D.listR 6 D.unsignedInt8
        |> D.andThen
            (\signature ->
                if signature == [ 97, 55, 56, 70, 73, 71 ] || signature == [ 97, 57, 56, 70, 73, 71 ] then
                    D.succeed ()

                else
                    D.fail
            )


decodePalette : Int -> D.Decoder (Array Int)
decodePalette bits =
    D.array (2 ^ bits) (D.unsignedInt24 BE)


decodeExtensionBlocks : D.Decoder { code : Int, body : List Bytes }
decodeExtensionBlocks =
    let
        bodyLoop acc =
            D.unsignedInt8
                |> D.andThen
                    (\count ->
                        if count > 0 then
                            D.bytes count
                                |> D.andThen (\a -> bodyLoop (a :: acc))

                        else
                            D.succeed acc
                    )
    in
    D.unsignedInt8
        |> D.andThen
            (\fnCode ->
                bodyLoop []
                    |> D.map
                        (\ext ->
                            { code = fnCode, body = ext }
                        )
            )


decodeFrames info (( ext, dsc ) as acc) =
    D.unsignedInt8
        |> D.andThen
            (\headSymbol ->
                if headSymbol == 0x21 then
                    decodeExtensionBlocks
                        |> D.andThen (\eee -> decodeFrames info ( eee :: ext, dsc ))

                else if headSymbol == 0x2C then
                    D.map5
                        (\left top w h mipx ->
                            { left = left
                            , top = top
                            , w = w
                            , h = h
                            , m = Bitwise.shiftRightBy 7 mipx
                            , px = Bitwise.and 7 mipx
                            , i = Bitwise.and 1 <| Bitwise.shiftRightBy 6 mipx
                            , data = []

                            --, background = info.background
                            --, palette = info.palette
                            }
                        )
                        (D.unsignedInt16 LE)
                        (D.unsignedInt16 LE)
                        (D.unsignedInt16 LE)
                        (D.unsignedInt16 LE)
                        D.unsignedInt8
                        |> D.andThen
                            (\dsc_ ->
                                D.map2
                                    (\firstCodeSize count_firstBlock ->
                                        Lzw.decoder (2 ^ info.bits - 1) (firstCodeSize + 1) count_firstBlock
                                    )
                                    D.unsignedInt8
                                    D.unsignedInt8
                                    |> D.andThen identity
                                    |> D.map (\data -> ( ext, Array.push { dsc_ | data = data } dsc ))
                            )

                else
                    D.succeed acc
            )


type alias GifInfo =
    { width : Int
    , height : Int
    , m : Int
    , cr : Int
    , bits : Int
    , background : Int
    }


decodeInfo : D.Decoder GifInfo
decodeInfo =
    -- 7 Bytes
    D.map4
        (\width height mCrPixel background ->
            let
                m =
                    Bitwise.shiftRightBy 7 mCrPixel

                cr =
                    Bitwise.and 7 <| Bitwise.shiftRightBy 4 mCrPixel

                pixel =
                    Bitwise.and 7 mCrPixel
            in
            { width = width
            , height = height
            , m = m
            , cr = cr + 1
            , bits = pixel + 1
            , background = background
            }
        )
        (D.unsignedInt16 LE)
        (D.unsignedInt16 LE)
        D.unsignedInt8
        D.unsignedInt8
        |> D.andThen (\info -> D.unsignedInt8 |> D.map (\skip -> info))
