module View exposing (View, cellBounds, view)

import Grid
import Helper exposing (Coord)
import Point2d
import Units


type View
    = View
        { viewPoint : Coord Units.AsciiUnit
        , viewSize : Coord Units.AsciiUnit
        }


view : { viewPoint : Coord Units.AsciiUnit, viewSize : Coord Units.AsciiUnit } -> View
view view_ =
    let
        viewPoint_ =
            Helper.minTuple ( Units.asciiUnit 100, Units.asciiUnit 100 ) view_.viewPoint
                |> Helper.maxTuple ( Units.asciiUnit -100, Units.asciiUnit -100 )

        maxSize =
            Point2d.xy (Units.worldUnit 4000) (Units.worldUnit 2200) |> Units.worldToAscii
    in
    View
        { viewPoint = viewPoint_
        , viewSize = Helper.absTuple view_.viewSize |> Helper.minTuple maxSize
        }


cellBounds : View -> { min : Coord Units.CellUnit, max : Coord Units.CellUnit }
cellBounds (View view_) =
    let
        ( sx, sy ) =
            Helper.toRawCoord view_.viewSize

        ( x, y ) =
            Helper.toRawCoord view_.viewPoint
    in
    { min =
        ( toFloat x - toFloat sx / 2 |> floor, toFloat y - toFloat sy / 2 |> floor )
            |> Helper.fromRawCoord
            |> Grid.asciiToCellAndLocalCoord
            |> Tuple.first
    , max =
        ( toFloat x - toFloat sx / 2 |> ceiling, toFloat y - toFloat sy / 2 |> ceiling )
            |> Helper.fromRawCoord
            |> Grid.asciiToCellAndLocalCoord
            |> Tuple.first
    }
