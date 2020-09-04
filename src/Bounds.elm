module Bounds exposing (Bounds, bounds, boundsToBounds2d, center, contains, containsBounds, convert, coordRangeFold, coordRangeFoldReverse, expand)

import BoundingBox2d exposing (BoundingBox2d)
import Helper exposing (Coord)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))


type Bounds unit
    = Bounds { min : Coord unit, max : Coord unit }


bounds : Coord unit -> Coord unit -> Bounds unit
bounds min_ max_ =
    Bounds
        { min = Helper.minTuple min_ max_
        , max = Helper.maxTuple min_ max_
        }


expand : Quantity Int unit -> Bounds unit -> Bounds unit
expand expandBy (Bounds bounds_) =
    Bounds
        { min = Helper.minusTuple ( expandBy, expandBy ) bounds_.min
        , max = Helper.addTuple ( expandBy, expandBy ) bounds_.max
        }


contains : Coord unit -> Bounds unit -> Bool
contains ( Quantity x, Quantity y ) (Bounds bounds_) =
    let
        ( Quantity minX, Quantity minY ) =
            bounds_.min

        ( Quantity maxX, Quantity maxY ) =
            bounds_.max
    in
    minX <= x && x < maxX && minY <= y && y < maxY


containsBounds : Bounds unit -> Bounds unit -> Bool
containsBounds (Bounds otherBounds) (Bounds bounds_) =
    let
        ( Quantity minX, Quantity minY ) =
            bounds_.min

        ( Quantity maxX, Quantity maxY ) =
            bounds_.max

        ( Quantity otherMinX, Quantity otherMinY ) =
            otherBounds.min

        ( Quantity otherMaxX, Quantity otherMaxY ) =
            otherBounds.max
    in
    (minX <= otherMinX && otherMinX <= maxX && minY <= otherMinY && otherMinY <= maxY)
        && (minX <= otherMaxX && otherMaxX <= maxX && minY <= otherMaxY && otherMaxY <= maxY)


convert : (Coord unit0 -> Coord unit1) -> Bounds unit0 -> Bounds unit1
convert convertFunc (Bounds bounds_) =
    Bounds
        { min = convertFunc bounds_.min
        , max = convertFunc bounds_.max
        }


center : Bounds unit -> Point2d unit coordinate
center (Bounds bounds_) =
    let
        ( minX, minY ) =
            bounds_.min

        ( maxX, maxY ) =
            bounds_.max
    in
    Point2d.xy
        (Quantity.plus minX maxX |> Quantity.toFloatQuantity |> Quantity.divideBy 2)
        (Quantity.plus minY maxY |> Quantity.toFloatQuantity |> Quantity.divideBy 2)


boundsToBounds2d : Bounds units -> BoundingBox2d units coordinate
boundsToBounds2d (Bounds bounds_) =
    BoundingBox2d.from (Helper.coordToPoint bounds_.min) (Helper.coordToPoint bounds_.max)


coordRangeFold : (Coord units -> a -> a) -> (a -> a) -> Bounds units -> a -> a
coordRangeFold foldFunc rowChangeFunc (Bounds bounds_) initialValue =
    let
        ( x0, y0 ) =
            Helper.toRawCoord bounds_.min

        ( x1, y1 ) =
            Helper.toRawCoord bounds_.max
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


coordRangeFoldReverse : (Coord units -> a -> a) -> (a -> a) -> Bounds units -> a -> a
coordRangeFoldReverse foldFunc rowChangeFunc (Bounds bounds_) initialValue =
    let
        ( x0, y0 ) =
            Helper.toRawCoord bounds_.min

        ( x1, y1 ) =
            Helper.toRawCoord bounds_.max
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
