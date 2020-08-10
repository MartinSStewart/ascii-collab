module Units exposing (AsciiUnit, CellUnit, WorldPixel, asciiToWorld, asciiUnit, cellUnit, inWorldUnits, worldUnit)

import Ascii
import Helper exposing (Coord)
import Pixels
import Quantity exposing (Quantity)


type WorldPixel
    = WorldPixel Never


type AsciiUnit
    = AsciiUnit Never


type CellUnit
    = CellUnit Never


asciiUnit : Int -> Quantity Int AsciiUnit
asciiUnit =
    Quantity.Quantity


worldUnit : Int -> Quantity Int WorldPixel
worldUnit =
    Quantity.Quantity


inWorldUnits : Quantity Int WorldPixel -> Int
inWorldUnits (Quantity.Quantity value) =
    value


cellUnit : Int -> Quantity Int CellUnit
cellUnit =
    Quantity.Quantity


asciiToWorld : Coord Int AsciiUnit -> Coord Int WorldPixel
asciiToWorld ( Quantity.Quantity x, Quantity.Quantity y ) =
    let
        ( w, h ) =
            Ascii.size
    in
    ( Quantity.Quantity (Pixels.inPixels w * x), Quantity.Quantity (Pixels.inPixels h * y) )
