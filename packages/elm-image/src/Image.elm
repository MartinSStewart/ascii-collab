module Image exposing
    ( Image
    , decode
    , toPng, toBmp, toPngUrl, toBmpUrl
    , fromList, fromList2d, fromArray, fromArray2d
    , toList, toList2d, toArray, toArray2d
    , dimensions
    , Pixel, Width, Height
    )

{-|

@docs Image


# Decoding

@docs decode


# Encoding

@docs toPng, toBmp, toPngUrl, toBmpUrl


# Construct

@docs fromList, fromList2d, fromArray, fromArray2d


# Destruct

@docs toList, toList2d, toArray, toArray2d


# Meta Data

@docs dimensions


# Helper Types

@docs Pixel, Width, Height

-}

import Array exposing (Array)
import Base64
import Bytes exposing (Bytes)
import Image.Advanced
import Image.Internal.BMP as BMP
import Image.Internal.GIF as GIF
import Image.Internal.ImageData exposing (Image(..), PixelFormat(..))
import Image.Internal.Meta exposing (FromDataBitDepth(..), FromDataColor(..), Header(..))
import Image.Internal.PNG as PNG
import Maybe exposing (Maybe)


{-| -}
type alias Width =
    Int


{-| -}
type alias Height =
    Int


{-| Color encoded as `0xRRGGBBAA`
-}
type alias Pixel =
    Int


{-| Data Model representing Image data, that can be used to create image, or convert primitives to use image pixels as data
-}
type alias Image =
    Image.Internal.ImageData.Image


{-| Create [`Image`](#Image) of `List Int` where each `Pixel` is `Int` as `0xRRGGBBAA`
-}
fromList : Width -> List Pixel -> Image
fromList w l =
    List
        (FromData
            { width = w
            , height = List.length l // w
            , color = FromDataChannel4 FromDataBitDepth8
            }
        )
        l


{-| Create [`Image`](#Image) of `List (List Int)` where each `Pixel` is `Int` as `0xRRGGBBAA`
-}
fromList2d : List (List Pixel) -> Image
fromList2d l =
    List2d
        (FromData
            { width = l |> List.head |> Maybe.map List.length |> Maybe.withDefault 0
            , height = List.length l
            , color = FromDataChannel4 FromDataBitDepth8
            }
        )
        l


{-| Create [`Image`](#Image) of `Array Int` where each `Pixel` is `Int` as `0xRRGGBBAA`
-}
fromArray : Width -> Array Pixel -> Image
fromArray w arr =
    Array
        (FromData
            { width = w
            , height = Array.length arr // w
            , color = FromDataChannel4 FromDataBitDepth8
            }
        )
        arr


{-| Create [`Image`](#Image) of `Array (Array Pixel)` where each `Pixel` is `Int` as `0xRRGGBBAA`
-}
fromArray2d : Array (Array Pixel) -> Image
fromArray2d arr =
    Array2d
        (FromData
            { width = arr |> Array.get 0 |> Maybe.map Array.length |> Maybe.withDefault 0
            , height = Array.length arr
            , color = FromDataChannel4 FromDataBitDepth8
            }
        )
        arr


{-| Take [`Image`](#Image) of and converts it to `List Pixel` where each `Pixel` is `Int` as `0xRRGGBBAA`
-}
toList : Image -> List Pixel
toList =
    Image.Internal.ImageData.toList


{-| Take [`Image`](#Image) of and converts it to matrix `List (List Pixel)` where each `Pixel` is `Int` as `0xRRGGBBAA`
-}
toList2d : Image -> List (List Pixel)
toList2d =
    Image.Internal.ImageData.toList2d


{-| Take [`Image`](#Image) of and converts it to `Array Pixel` where each `Pixel` is `Int` as `0xRRGGBBAA`
-}
toArray : Image -> Array Pixel
toArray =
    Image.Internal.ImageData.toArray


{-| Take [`Image`](#Image) of and converts it to matrix `Array (Array Pixel)` where each `Pixel` is `Int` as `0xRRGGBBAA`
-}
toArray2d : Image -> Array (Array Pixel)
toArray2d =
    Image.Internal.ImageData.toArray2d


{-| Portable Network Graphics (PNG) is a raster-graphics file-format that supports lossless data compression. PNG was developed as an improved, non-patented replacement for Graphics Interchange Format (GIF).

PNG supports palette-based images (with palettes of 24-bit RGB or 32-bit RGBA colors), grayscale images (with or without alpha channel for transparency), and full-color non-palette-based RGB images (with or without alpha channel).

-}
toPng : Image -> Bytes
toPng =
    Image.Advanced.toPng32


{-| Create base64-url that can be used directly as img source (`img [ src <| toPngUrl myImage ]`)
-}
toPngUrl : Image -> String
toPngUrl =
    Image.Advanced.toPng32 >> Base64.fromBytes >> Maybe.withDefault "" >> (++) "data:image/png;base64,"


{-| The BMP file format, also known as bitmap image file or device independent bitmap (DIB) file format or simply a bitmap, is a raster graphics image file format used to store bitmap digital images, independently of the display device (such as a graphics adapter), especially on Microsoft Windows and OS/2 operating systems.

**Note**: Using BMP 32bit is discouraged due to lack of proper support across browsers, so image will be 24bit (no alpha channel)

-}
toBmp : Image -> Bytes
toBmp =
    Image.Advanced.toBmp24


{-| Create base64-url that can be used directly as img source (`img [ src <| toBmpUrl myImage ]`)
-}
toBmpUrl : Image -> String
toBmpUrl =
    Image.Advanced.toBmp24 >> Base64.fromBytes >> Maybe.withDefault "" >> (++) "data:image/bmp;base64,"


{-| Convert blob of image (`png` or `bmp`) into [`Image`](#Image)

    import Http

    type Msg
        = GotImage (Result Http.Error (Maybe Image))

    getImage : Cmd Msg
    getImage =
        Http.get
            { url = "/image.png"
            , expect = Http.expectBytes GotImage Image.decode
            }

-}
decode : Bytes -> Maybe Image
decode bytes =
    PNG.decode bytes
        |> orThenLazy (\_ -> BMP.decode bytes)
        |> orThenLazy (\_ -> GIF.decode bytes)


{-| Get `width` and `height` of [`Image`](#Image)
-}
dimensions : Image -> { width : Int, height : Int }
dimensions =
    Image.Internal.ImageData.dimensions


orThenLazy : (() -> Maybe a) -> Maybe a -> Maybe a
orThenLazy ma mb =
    case mb of
        Nothing ->
            ma ()

        Just _ ->
            mb
