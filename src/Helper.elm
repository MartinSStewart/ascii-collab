module Helper exposing (Coord, addTuple, coordToVec, multiplyTuple)

import Math.Vector2 exposing (Vec2)
import Quantity exposing (Quantity(..))


addTuple : Coord number unit -> Coord number unit -> Coord number unit
addTuple ( x0, y0 ) ( x1, y1 ) =
    ( Quantity.plus x0 x1, Quantity.plus y0 y1 )


multiplyTuple : ( number, number ) -> Coord number unit -> Coord number unit
multiplyTuple ( x0, y0 ) ( x1, y1 ) =
    ( Quantity.multiplyBy x0 x1, Quantity.multiplyBy y0 y1 )


coordToVec : Coord Int units -> Vec2
coordToVec ( Quantity x, Quantity y ) =
    Math.Vector2.vec2 (toFloat x) (toFloat y)


type alias Coord number units =
    ( Quantity number units, Quantity number units )
