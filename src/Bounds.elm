module Bounds exposing (Bounds, center, contains, convert)

import Helper exposing (Coord)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))


type alias Bounds unit =
    { min : Coord unit, max : Coord unit }


contains : Coord unit -> Bounds unit -> Bool
contains ( Quantity x, Quantity y ) bounds =
    let
        ( Quantity minX, Quantity minY ) =
            bounds.min

        ( Quantity maxX, Quantity maxY ) =
            bounds.max
    in
    minX <= x && x < maxX && minY <= y && y < maxY


convert : (Coord unit0 -> Coord unit1) -> Bounds unit0 -> Bounds unit1
convert convertFunc bounds =
    { min = convertFunc bounds.min
    , max = convertFunc bounds.max
    }


center : Bounds unit -> Point2d unit coordinate
center bounds =
    let
        ( minX, minY ) =
            bounds.min

        ( maxX, maxY ) =
            bounds.max
    in
    Point2d.xy
        (Quantity.plus minX maxX |> Quantity.toFloatQuantity |> Quantity.divideBy 2)
        (Quantity.plus minY maxY |> Quantity.toFloatQuantity |> Quantity.divideBy 2)
