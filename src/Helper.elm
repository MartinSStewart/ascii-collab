module Helper exposing (Coord, addTuple, coordRangeFold, coordRangeFoldReverse, coordToVec, maxTuple, minTuple, minusTuple, multiplyTuple, rawCoord)

import Math.Vector2 exposing (Vec2)
import Quantity exposing (Quantity(..))


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


coordToVec : Coord units -> Vec2
coordToVec ( Quantity x, Quantity y ) =
    Math.Vector2.vec2 (toFloat x) (toFloat y)


rawCoord : Coord units -> ( Int, Int )
rawCoord ( Quantity x, Quantity y ) =
    ( x, y )


coordRangeFold : (Coord units -> a -> a) -> (a -> a) -> Coord units -> Coord units -> a -> a
coordRangeFold foldFunc rowChangeFunc minCoord maxCoord initialValue =
    let
        ( x0, y0 ) =
            rawCoord (minTuple minCoord maxCoord)

        ( x1, y1 ) =
            rawCoord (maxTuple minCoord maxCoord)
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
            rawCoord (minTuple minCoord maxCoord)

        ( x1, y1 ) =
            rawCoord (maxTuple minCoord maxCoord)
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
