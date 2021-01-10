module Helper exposing
    ( Coord
    , RawCellCoord
    , absTuple
    , addTuple
    , area
    , coordToPoint
    , coordToVec
    , coordToVector2d
    , divideTuple
    , fromRawCoord
    , maxTuple
    , minTuple
    , minusTuple
    , multiplyTuple
    , roundPoint
    , toRawCoord
    , toggleSet
    )

import EverySet exposing (EverySet)
import Math.Vector2 exposing (Vec2)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))
import Vector2d exposing (Vector2d)


type alias RawCellCoord =
    ( Int, Int )


area : Coord unit -> Int
area coord =
    let
        ( x, y ) =
            toRawCoord coord
    in
    x * y


addTuple : Coord unit -> Coord unit -> Coord unit
addTuple ( x0, y0 ) ( x1, y1 ) =
    ( Quantity.plus x0 x1, Quantity.plus y0 y1 )


minusTuple : Coord unit -> Coord unit -> Coord unit
minusTuple ( x0, y0 ) ( x1, y1 ) =
    ( Quantity.minus x0 x1, Quantity.minus y0 y1 )


multiplyTuple : ( Int, Int ) -> Coord unit -> Coord unit
multiplyTuple ( x0, y0 ) ( x1, y1 ) =
    ( Quantity.multiplyBy x0 x1, Quantity.multiplyBy y0 y1 )


divideTuple : Coord unit -> Coord unit -> Coord unit
divideTuple ( Quantity x0, Quantity y0 ) ( Quantity x1, Quantity y1 ) =
    ( x1 // x0 |> Quantity, y1 // y0 |> Quantity )


minTuple : Coord unit -> Coord unit -> Coord unit
minTuple ( x0, y0 ) ( x1, y1 ) =
    ( Quantity.min x0 x1, Quantity.min y0 y1 )


maxTuple : Coord unit -> Coord unit -> Coord unit
maxTuple ( x0, y0 ) ( x1, y1 ) =
    ( Quantity.max x0 x1, Quantity.max y0 y1 )


absTuple : Coord unit -> Coord unit
absTuple ( x0, y0 ) =
    ( Quantity.abs x0, Quantity.abs y0 )


coordToVec : Coord units -> Vec2
coordToVec ( Quantity x, Quantity y ) =
    Math.Vector2.vec2 (toFloat x) (toFloat y)


coordToPoint : Coord units -> Point2d units coordinate
coordToPoint ( x, y ) =
    Point2d.xy (Quantity.toFloatQuantity x) (Quantity.toFloatQuantity y)


roundPoint : Point2d units coordinate -> Coord units
roundPoint point2d =
    let
        { x, y } =
            Point2d.unwrap point2d
    in
    fromRawCoord ( round x, round y )


coordToVector2d : Coord units -> Vector2d units coordinate
coordToVector2d ( x, y ) =
    Vector2d.xy (Quantity.toFloatQuantity x) (Quantity.toFloatQuantity y)


toRawCoord : Coord units -> ( Int, Int )
toRawCoord ( Quantity x, Quantity y ) =
    ( x, y )


fromRawCoord : ( Int, Int ) -> Coord units
fromRawCoord ( x, y ) =
    ( Quantity x, Quantity y )


type alias Coord units =
    ( Quantity Int units, Quantity Int units )


toggleSet : a -> EverySet a -> EverySet a
toggleSet value set =
    if EverySet.member value set then
        EverySet.remove value set

    else
        EverySet.insert value set
