module Units exposing (AsciiUnit, CellUnit, WorldPixel, asciiUnit, cellUnit, inWorldUnits, worldUnit)

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
