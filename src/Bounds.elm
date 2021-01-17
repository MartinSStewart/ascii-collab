module Bounds exposing
    ( Bounds
    , addToMax
    , bounds
    , boundsToBounds2d
    , center
    , contains
    , containsBounds
    , convert
    , coordRangeFold
    , coordRangeFoldReverse
    , expand
    , fromCoords
    , height
    , maximum
    , minimum
    , multiplyBy
    , translate
    , width
    )

import BoundingBox2d exposing (BoundingBox2d)
import Helper exposing (Coord)
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import NonemptyExtra as Nonempty
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))


type Bounds unit
    = Bounds { min : Coord unit, max : Coord unit }


width : Bounds unit -> Quantity Int unit
width bounds_ =
    maximum bounds_ |> Helper.minusTuple (minimum bounds_) |> Tuple.first


height : Bounds unit -> Quantity Int unit
height bounds_ =
    maximum bounds_ |> Helper.minusTuple (minimum bounds_) |> Tuple.second


minimum : Bounds unit -> Coord unit
minimum (Bounds bounds_) =
    bounds_.min


maximum : Bounds unit -> Coord unit
maximum (Bounds bounds_) =
    bounds_.max


multiplyBy : Int -> Bounds unit -> Bounds unit
multiplyBy scalar (Bounds bounds_) =
    Bounds
        { min = Helper.multiplyTuple ( scalar, scalar ) bounds_.min
        , max = Helper.multiplyTuple ( scalar, scalar ) bounds_.max
        }


bounds : Coord unit -> Coord unit -> Bounds unit
bounds min_ max_ =
    Bounds
        { min = Helper.minTuple min_ max_
        , max = Helper.maxTuple min_ max_
        }


fromCoords : Nonempty (Coord unit) -> Bounds unit
fromCoords coords =
    let
        xValues =
            List.Nonempty.map Tuple.first coords

        yValues =
            List.Nonempty.map Tuple.second coords
    in
    Bounds
        { min = ( Nonempty.minimumBy Quantity.unwrap xValues, Nonempty.minimumBy Quantity.unwrap yValues )
        , max = ( Nonempty.maximumBy Quantity.unwrap xValues, Nonempty.maximumBy Quantity.unwrap yValues )
        }


centerAndHalfSize : Coord unit -> Coord unit -> Bounds unit
centerAndHalfSize centerPoint halfSize =
    bounds
        (centerPoint |> Helper.minusTuple halfSize)
        (centerPoint |> Helper.addTuple halfSize)


translate : Coord unit -> Bounds unit -> Bounds unit
translate coord (Bounds bounds_) =
    Bounds
        { min = Helper.addTuple coord bounds_.min
        , max = Helper.addTuple coord bounds_.max
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
    minX <= x && x <= maxX && minY <= y && y <= maxY


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


addToMax : Coord unit -> Bounds unit -> Bounds unit
addToMax coord (Bounds bounds_) =
    Bounds { min = bounds_.min, max = Helper.addTuple coord bounds_.max }


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
            (if x >= maxX then
                minX

             else
                x + 1
            )
            (if x >= maxX then
                y + 1

             else
                y
            )
            (foldFunc ( Quantity x, Quantity y ) value
                |> (if x >= maxX && y < maxY then
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
