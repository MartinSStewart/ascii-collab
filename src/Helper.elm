module Helper exposing (Coord, absTuple, addTuple, boundsToBounds2d, coordRangeFold, coordRangeFoldReverse, coordToPoint, coordToVec, coordToVector2d, fromRawCoord, maxTuple, minTuple, minusTuple, multiplyTuple, roundPoint, toRawCoord)

import BoundingBox2d exposing (BoundingBox2d)
import Math.Vector2 exposing (Vec2)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))
import Vector2d exposing (Vector2d)


addTuple : Coord unit -> Coord unit -> Coord unit
addTuple ( x0, y0 ) ( x1, y1 ) =
    ( Quantity.plus x0 x1, Quantity.plus y0 y1 )


minusTuple : Coord unit -> Coord unit -> Coord unit
minusTuple ( x0, y0 ) ( x1, y1 ) =
    ( Quantity.minus x0 x1, Quantity.minus y0 y1 )


multiplyTuple : ( Int, Int ) -> Coord unit -> Coord unit
multiplyTuple ( x0, y0 ) ( x1, y1 ) =
    ( Quantity.multiplyBy x0 x1, Quantity.multiplyBy y0 y1 )


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


boundsToBounds2d : { min : Coord units, max : Coord units } -> BoundingBox2d units coordinate
boundsToBounds2d bounds =
    BoundingBox2d.from (coordToPoint bounds.min) (coordToPoint bounds.max)


coordRangeFold : (Coord units -> a -> a) -> (a -> a) -> Coord units -> Coord units -> a -> a
coordRangeFold foldFunc rowChangeFunc minCoord maxCoord initialValue =
    let
        ( x0, y0 ) =
            toRawCoord (minTuple minCoord maxCoord)

        ( x1, y1 ) =
            toRawCoord (maxTuple minCoord maxCoord)
    in
    coordRangeFoldHelper foldFunc rowChangeFunc x0 x1 y0 y1 x0 y0 initialValue


coordRangeFoldHelper : (Coord units -> a -> a) -> (a -> a) -> Int -> Int -> Int -> Int -> Int -> Int -> a -> a
coordRangeFoldHelper foldFunc rowChangeFunc minX maxX minY maxY x y value =
    if y > maxY then
        value

    else
        coordRangeFoldHelper foldFunc
            rowChangeFunc
            minX
            maxX
            minY
            maxY
            (if x > maxX then
                minX

             else
                x + 1
            )
            (if x > maxX then
                y + 1

             else
                y
            )
            (foldFunc ( Quantity x, Quantity y ) value
                |> (if x > maxX && y < maxY then
                        rowChangeFunc

                    else
                        identity
                   )
            )


coordRangeFoldReverse : (Coord units -> a -> a) -> (a -> a) -> Coord units -> Coord units -> a -> a
coordRangeFoldReverse foldFunc rowChangeFunc minCoord maxCoord initialValue =
    let
        ( x0, y0 ) =
            toRawCoord (minTuple minCoord maxCoord)

        ( x1, y1 ) =
            toRawCoord (maxTuple minCoord maxCoord)
    in
    coordRangeFoldReverseHelper foldFunc rowChangeFunc x0 x1 y0 y1 x1 y1 initialValue


coordRangeFoldReverseHelper : (Coord units -> a -> a) -> (a -> a) -> Int -> Int -> Int -> Int -> Int -> Int -> a -> a
coordRangeFoldReverseHelper foldFunc rowChangeFunc minX maxX minY maxY x y value =
    if y < minY then
        value

    else
        coordRangeFoldReverseHelper foldFunc
            rowChangeFunc
            minX
            maxX
            minY
            maxY
            (if x <= minX then
                maxX

             else
                x - 1
            )
            (if x <= minX then
                y - 1

             else
                y
            )
            (foldFunc ( Quantity x, Quantity y ) value
                |> (if x <= minX && y > minY then
                        rowChangeFunc

                    else
                        identity
                   )
            )


type alias Coord units =
    ( Quantity Int units, Quantity Int units )
