module Image.Internal.Meta exposing
    ( BitDepth1_2_4_8(..)
    , BitDepth1_2_4_8_16(..)
    , BitDepth8_16(..)
    , BmpBitsPerPixel(..)
    , BmpHeader
    , FromDataBitDepth(..)
    , FromDataColor(..)
    , FromDataInfo
    , GifHeader
    , Header(..)
    , PngColor(..)
    , PngHeader
    , dimensions
    )

{-| -}

import Bytes exposing (Bytes)
import Dict exposing (Dict)


{-| -}
dimensions : Header -> { width : Int, height : Int }
dimensions meta =
    case meta of
        Png { width, height } ->
            { width = width, height = height }

        Bmp { width, height } ->
            { width = width, height = height }

        Gif { width, height } ->
            { width = width, height = height }

        FromData { width, height } ->
            { width = width, height = height }


type Header
    = Png PngHeader
    | Bmp BmpHeader
    | Gif GifHeader
    | FromData FromDataInfo


{-| -}
type alias FromDataInfo =
    { width : Int
    , height : Int
    , color : FromDataColor
    }


{-| -}
type alias PngHeader =
    { width : Int
    , height : Int
    , color : PngColor
    , adam7 : Bool
    , chunks : Dict String Bytes
    }


{-| -}
type alias BmpHeader =
    { width : Int
    , height : Int
    , fileSize : Int
    , pixelStart : Int
    , dibHeader : Int
    , color_planes : Int
    , bitsPerPixel : BmpBitsPerPixel
    , compression : Int
    , dataSize : Int
    }


{-| -}
type alias GifHeader =
    { width : Int
    , height : Int
    }


{-| -}
type FromDataColor
    = FromDataChannel1 FromDataBitDepth
    | FromDataChannel2 FromDataBitDepth
    | FromDataChannel3 FromDataBitDepth
    | FromDataChannel4 FromDataBitDepth


{-| -}
type FromDataBitDepth
    = FromDataBitDepth1
    | FromDataBitDepth2
    | FromDataBitDepth4
    | FromDataBitDepth8
    | FromDataBitDepth16


{-| -}
type BmpBitsPerPixel
    = BmpBitsPerPixel8
    | BmpBitsPerPixel16
    | BmpBitsPerPixel24
    | BmpBitsPerPixel32


{-| -}
type PngColor
    = Greyscale BitDepth1_2_4_8_16
    | GreyscaleAlpha BitDepth8_16
    | TrueColour BitDepth8_16
    | TrueColourAlpha BitDepth8_16
    | IndexedColour BitDepth1_2_4_8


{-| -}
type BitDepth1_2_4_8_16
    = BitDepth1_2_4_8_16__1
    | BitDepth1_2_4_8_16__2
    | BitDepth1_2_4_8_16__4
    | BitDepth1_2_4_8_16__8
    | BitDepth1_2_4_8_16__16


{-| -}
type BitDepth1_2_4_8
    = BitDepth1_2_4_8__1
    | BitDepth1_2_4_8__2
    | BitDepth1_2_4_8__4
    | BitDepth1_2_4_8__8


{-| -}
type BitDepth8_16
    = BitDepth8_16__8
    | BitDepth8_16__16
