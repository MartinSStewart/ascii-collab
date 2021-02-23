module Image.Advanced exposing
    ( getType, ImageType
    , map, eval
    , toPng32
    , toBmp24, toBmp32
    )

{-|


# Image Info

@docs getType, ImageType


# Manipulations

@docs map, eval


# Custom Encoding

@docs toPng32
@docs toBmp24, toBmp32

-}

import Bytes exposing (Bytes)
import Image.Internal.BMP as BMP
import Image.Internal.ImageData as ImageData exposing (Image, PixelFormat(..))
import Image.Internal.Meta exposing (BmpHeader, FromDataBitDepth(..), FromDataColor(..), Header(..), PngHeader)
import Image.Internal.PNG as PNG
import Image.Internal.Pixel as Pixel


{-| Possible image decoded type
-}
type ImageType
    = PNG
    | BMP
    | GIF
    | SCR


{-| get image type
-}
getType : ImageData.Image -> ImageType
getType image =
    case ImageData.getInfo image of
        Png _ ->
            PNG

        Bmp _ ->
            BMP

        Gif _ ->
            GIF

        FromData _ ->
            SCR


{-| Apply a function on every pixel in an image.
-}
map : (Int -> Int) -> Image -> Image
map =
    ImageData.map


{-| When decoding images they are decoded in "lazy way" (real decoding is postponed until data is needed)
this function do decode, useful if you need to encode multiple images from same source
-}
eval : Image -> Image
eval =
    ImageData.eval


{-| Encode image into True color with alpha PNG image
-}
toPng32 : Image -> Bytes
toPng32 =
    Pixel.toBit32 >> PNG.encode


{-| Encode image into BMP24
-}
toBmp24 : Image -> Bytes
toBmp24 =
    Pixel.toBit24
        >> ImageData.forceColor (FromDataChannel3 FromDataBitDepth8)
        >> BMP.encode


{-| Encode image into BMP32

**Note**: Using BMP 32bit is discouraged due to lack of proper support across browsers

-}
toBmp32 : Image -> Bytes
toBmp32 =
    Pixel.toBit32
        >> ImageData.forceColor (FromDataChannel4 FromDataBitDepth8)
        >> BMP.encode
