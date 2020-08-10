module Units exposing (AsciiUnit, CellUnit, ScreenCoordinate, WorldCoordinate, WorldPixel, asciiToWorld, asciiUnit, cellUnit, inWorldUnits, pixelToWorldPixel, screenFrame, worldToAscii, worldUnit)

import Ascii
import Axis2d
import Frame2d exposing (Frame2d)
import Helper exposing (Coord)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Vector2d exposing (Vector2d)


type WorldPixel
    = WorldPixel Never


type AsciiUnit
    = AsciiUnit Never


type CellUnit
    = CellUnit Never


asciiUnit : Int -> Quantity Int AsciiUnit
asciiUnit =
    Quantity.Quantity


worldUnit : number -> Quantity number WorldPixel
worldUnit =
    Quantity.Quantity


inWorldUnits : Quantity Int WorldPixel -> Int
inWorldUnits (Quantity.Quantity value) =
    value


cellUnit : Int -> Quantity Int CellUnit
cellUnit =
    Quantity.Quantity


asciiToWorld : Coord AsciiUnit -> Coord WorldPixel
asciiToWorld ( Quantity.Quantity x, Quantity.Quantity y ) =
    let
        ( w, h ) =
            Ascii.size
    in
    ( Quantity.Quantity (Pixels.inPixels w * x), Quantity.Quantity (Pixels.inPixels h * y) )


worldToAscii : Point2d WorldPixel WorldCoordinate -> Coord AsciiUnit
worldToAscii point =
    let
        ( w, h ) =
            Ascii.size

        { x, y } =
            Point2d.unwrap point
    in
    ( Quantity.Quantity (x / Pixels.inPixels w |> floor), Quantity.Quantity (y / Pixels.inPixels h |> floor) )


pixelToWorldPixel : Float -> Vector2d Pixels ScreenCoordinate -> Coord WorldPixel
pixelToWorldPixel devicePixelRatio v =
    let
        { x, y } =
            Vector2d.unwrap v
    in
    ( x * devicePixelRatio |> round |> worldUnit, y * devicePixelRatio |> round |> worldUnit )


screenFrame : Point2d WorldPixel WorldCoordinate -> Frame2d WorldPixel WorldCoordinate { defines : ScreenCoordinate }
screenFrame viewPoint =
    Frame2d.atPoint viewPoint


type ScreenCoordinate
    = ScreenCoordinate Never


type WorldCoordinate
    = WorldCoordinate Never
