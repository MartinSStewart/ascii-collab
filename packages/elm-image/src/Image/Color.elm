module Image.Color exposing
    ( fromList, fromList2d, fromArray, fromArray2d
    , toList, toList2d, toArray, toArray2d
    )

{-| Same as main functions only instead of `Int` uses `Color` (`avh4/elm-color`) to represent pixel


# Construct

@docs fromList, fromList2d, fromArray, fromArray2d


# Destruct

@docs toList, toList2d, toArray, toArray2d

-}

import Array exposing (Array)
import Bitwise
import Color exposing (Color)
import Image exposing (Image, Width)
import Image.Internal.ImageData exposing (Image(..))


{-| Create [`Image`](Image#Image) of `List Color` where each `Color` is pixel
-}
fromList : Width -> List Color -> Image
fromList w =
    List.map colorToInt32 >> Image.fromList w


{-| Create [`Image`](#Image) of `List (List Int)` where each `Color` is pixel
-}
fromList2d : List (List Color) -> Image
fromList2d =
    List.map (List.map colorToInt32) >> Image.fromList2d


{-| Create [`Image`](#Image) of `Array Int` where each `Color` is pixel
-}
fromArray : Width -> Array Color -> Image
fromArray w =
    Array.map colorToInt32 >> Image.fromArray w


{-| Create [`Image`](#Image) of `Array (Array Int)` where each `Color` is pixel
-}
fromArray2d : Array (Array Color) -> Image
fromArray2d =
    Array.map (Array.map colorToInt32) >> Image.fromArray2d


{-| Take [`Image`](#Image) of and converts it to `List Int` where each `Color` is pixel
-}
toList : Image -> List Color
toList =
    Image.Internal.ImageData.toList >> List.map int32ToColor


{-| Take [`Image`](#Image) of and converts it to matrix `List (List Int)` where each `Color` is pixel
-}
toList2d : Image -> List (List Color)
toList2d =
    Image.Internal.ImageData.toList2d >> List.map (List.map int32ToColor)


{-| Take [`Image`](#Image) of and converts it to `Array Int` where each `Color` is pixel
-}
toArray : Image -> Array Color
toArray =
    Image.Internal.ImageData.toArray >> Array.map int32ToColor


{-| Take [`Image`](#Image) of and converts it to matrix `Array (Array Int)` where each `Color` is pixel
-}
toArray2d : Image -> Array (Array Color)
toArray2d =
    Image.Internal.ImageData.toArray2d >> Array.map (Array.map int32ToColor)


int32ToColor : Int -> Color
int32ToColor int =
    let
        a =
            int
                |> Bitwise.and 0xFF
                |> toFloat

        b =
            int
                |> Bitwise.shiftRightBy 8
                |> Bitwise.and 0xFF
                |> toFloat

        g =
            int
                |> Bitwise.shiftRightBy 16
                |> Bitwise.and 0xFF
                |> toFloat

        r =
            int
                |> Bitwise.shiftRightZfBy 24
                |> Bitwise.and 0xFF
                |> toFloat
    in
    Color.rgba (r / 255) (g / 255) (b / 255) (a / 255)


colorToInt32 : Color -> Int
colorToInt32 color =
    let
        record =
            Color.toRgba color

        byte1 =
            (record.alpha * 255)
                |> round

        byte2 =
            (record.blue * 255)
                |> round
                |> Bitwise.shiftLeftBy 8

        byte3 =
            (record.green * 255)
                |> round
                |> Bitwise.shiftLeftBy 16

        byte4 =
            (record.red * 255)
                |> round
                |> Bitwise.shiftLeftBy 24
    in
    Bitwise.or byte1 byte2
        |> Bitwise.or byte3
        |> Bitwise.or byte4
        |> Bitwise.shiftRightZfBy 0
