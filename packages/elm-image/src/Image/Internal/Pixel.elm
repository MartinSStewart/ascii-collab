module Image.Internal.Pixel exposing (toBit24, toBit32)

import Bitwise
import Image.Internal.ImageData as ImageData exposing (Image)
import Image.Internal.Meta exposing (BmpBitsPerPixel(..), FromDataBitDepth(..), FromDataColor(..), Header(..), PngColor(..))


toBit24 : Image -> Image
toBit24 image =
    case ImageData.getInfo image of
        Png { color } ->
            case color of
                Greyscale _ ->
                    dropChannel image

                TrueColour _ ->
                    dropChannel image

                IndexedColour _ ->
                    dropChannel image

                GreyscaleAlpha _ ->
                    ImageData.map px8AlphaTo24 image

                TrueColourAlpha _ ->
                    dropChannel image

        Bmp { bitsPerPixel } ->
            case bitsPerPixel of
                BmpBitsPerPixel8 ->
                    ImageData.map px8to24 image

                BmpBitsPerPixel16 ->
                    image

                BmpBitsPerPixel24 ->
                    image

                BmpBitsPerPixel32 ->
                    dropChannel image

        Gif _ ->
            image

        FromData { color } ->
            case color of
                FromDataChannel1 _ ->
                    image

                FromDataChannel2 _ ->
                    image

                FromDataChannel3 _ ->
                    image

                FromDataChannel4 FromDataBitDepth8 ->
                    dropChannel image

                FromDataChannel4 _ ->
                    image


toBit32 : Image -> Image
toBit32 image =
    case ImageData.getInfo image of
        Png { color } ->
            case color of
                Greyscale _ ->
                    image

                TrueColour _ ->
                    image

                IndexedColour _ ->
                    image

                GreyscaleAlpha _ ->
                    ImageData.map px8AlphaTo32 image

                TrueColourAlpha _ ->
                    image

        Bmp { bitsPerPixel } ->
            case bitsPerPixel of
                BmpBitsPerPixel8 ->
                    ImageData.map px8to32 image

                BmpBitsPerPixel16 ->
                    image

                BmpBitsPerPixel24 ->
                    addAlphaChannel image

                BmpBitsPerPixel32 ->
                    image

        Gif _ ->
            image
                |> ImageData.map
                    (\c ->
                        if c == 0 then
                            c

                        else
                            (Bitwise.shiftLeftBy 8 >> (+) 0xFF) c
                    )

        FromData { color } ->
            case color of
                FromDataChannel1 _ ->
                    image

                FromDataChannel2 _ ->
                    image

                FromDataChannel3 _ ->
                    image

                FromDataChannel4 _ ->
                    image


px8to24 : Int -> Int
px8to24 px =
    px
        |> Bitwise.shiftLeftBy 8
        |> (+) px
        |> Bitwise.shiftLeftBy 8
        |> (+) px
        |> Bitwise.shiftLeftBy 8
        |> (+) px


px8AlphaTo24 : Int -> Int
px8AlphaTo24 px_ =
    let
        px =
            Bitwise.shiftRightZfBy 8 px_
    in
    px
        |> Bitwise.shiftLeftBy 8
        |> (+) px
        |> Bitwise.shiftLeftBy 8
        |> (+) px


px8to32 : Int -> Int
px8to32 px =
    px
        |> Bitwise.shiftLeftBy 8
        |> (+) px
        |> Bitwise.shiftLeftBy 8
        |> (+) px
        |> Bitwise.shiftLeftBy 8
        |> (+) px
        |> Bitwise.shiftLeftBy 8
        |> (+) 0xFF


px8AlphaTo32 : Int -> Int
px8AlphaTo32 px_ =
    let
        px =
            Bitwise.shiftRightZfBy 8 px_
    in
    px
        |> Bitwise.shiftLeftBy 8
        |> (+) px
        |> Bitwise.shiftLeftBy 8
        |> (+) px
        |> Bitwise.shiftLeftBy 8
        |> (+) px
        |> Bitwise.shiftLeftBy 8
        |> (+) (Bitwise.and 0xFF px_)


dropChannel : Image -> Image
dropChannel =
    ImageData.map (Bitwise.shiftRightZfBy 8)


addAlphaChannel : Image -> Image
addAlphaChannel =
    ImageData.map (Bitwise.shiftLeftBy 8 >> (+) 0xFF)
