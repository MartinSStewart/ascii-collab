module AsciiItensity exposing (..)

import Array exposing (Array)
import Ascii exposing (Ascii)
import Base64
import Bounds
import Helper
import Html exposing (Html)
import Html.Attributes
import Image exposing (Image)
import List.Nonempty


intensities : Maybe (List ( Ascii, Int ))
intensities =
    Ascii.textureData
        |> String.dropLeft (String.length "data:image/png;base64,")
        |> Base64.toBytes
        |> Maybe.andThen Image.decode
        |> Maybe.map
            (\image ->
                let
                    pixelArray : Array (Array Image.Pixel)
                    pixelArray =
                        Image.toArray2d image
                in
                List.Nonempty.foldl
                    (\ascii acc ->
                        ( ascii
                        , Bounds.coordRangeFold
                            (\coord total ->
                                let
                                    ( x, y ) =
                                        Helper.toRawCoord coord
                                in
                                Array.get y pixelArray
                                    |> Maybe.andThen (Array.get x)
                                    |> Maybe.map
                                        (\pixel ->
                                            if pixel < 0 then
                                                1

                                            else
                                                0
                                        )
                                    |> Maybe.withDefault -255
                                    |> (+) total
                            )
                            identity
                            (Ascii.texturePositionInt ascii)
                            0
                        )
                            :: acc
                    )
                    []
                    Ascii.asciis
            )


main : Html ()
main =
    Html.div
        [ Html.Attributes.style "font-family" "courier"
        , Html.Attributes.style "white-space" "pre-wrap"
        ]
        [ Html.text
            (case intensities of
                Just intensities_ ->
                    let
                        patterns : String
                        patterns =
                            intensities_
                                |> List.map (\( ascii, intensity ) -> "        Ascii " ++ String.fromInt (Ascii.toInt ascii) ++ " -> " ++ String.fromInt intensity ++ "\n\n")
                                |> String.concat
                    in
                    """{-| Generated with http://localhost:8000/tools/AsciiItensity.elm -}
intensity : Ascii -> Int
intensity ascii =
    case ascii of 
"""
                        ++ patterns
                        ++ "        _ -> 0"

                Nothing ->
                    "Something went wrong"
            )
        ]
