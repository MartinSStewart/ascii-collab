module Units exposing (AsciiUnit(..), CellUnit(..), ScreenCoordinate(..), WorldCoordinate(..), WorldPixel(..), asciiToWorld, asciiUnit, cellToAscii, cellToAscii_, cellUnit, inWorldUnits, pixelToWorldPixel, screenFrame, worldToAscii, worldUnit)

import Ascii
import Frame2d exposing (Frame2d)
import GridCell
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


asciiUnit : number -> Quantity number AsciiUnit
asciiUnit =
    Quantity.Quantity


worldUnit : number -> Quantity number WorldPixel
worldUnit =
    Quantity.Quantity


inWorldUnits : Quantity Int WorldPixel -> Int
inWorldUnits (Quantity.Quantity value) =
    value


cellUnit : number -> Quantity number CellUnit
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


cellToAscii : Coord CellUnit -> Coord AsciiUnit
cellToAscii coord =
    Helper.multiplyTuple ( GridCell.cellSize, GridCell.cellSize ) coord |> Helper.toRawCoord |> Helper.fromRawCoord


cellToAscii_ : Point2d CellUnit WorldCoordinate -> Point2d AsciiUnit WorldCoordinate
cellToAscii_ coord =
    coord |> Point2d.at (Quantity.per (cellUnit 1) (asciiUnit (toFloat GridCell.cellSize)))


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
