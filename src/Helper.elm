module Helper exposing (Coord, addTuple, coordToVec, multiplyTuple, rawCoord)

import Math.Vector2 exposing (Vec2)
import Quantity exposing (Quantity(..))


addTuple : Coord unit -> Coord unit -> Coord unit
addTuple ( x0, y0 ) ( x1, y1 ) =
    ( Quantity.plus x0 x1, Quantity.plus y0 y1 )


multiplyTuple : ( Int, Int ) -> Coord unit -> Coord unit
multiplyTuple ( x0, y0 ) ( x1, y1 ) =
    ( Quantity.multiplyBy x0 x1, Quantity.multiplyBy y0 y1 )


coordToVec : Coord units -> Vec2
coordToVec ( Quantity x, Quantity y ) =
    Math.Vector2.vec2 (toFloat x) (toFloat y)


rawCoord : Coord units -> ( Int, Int )
rawCoord ( Quantity x, Quantity y ) =
    ( x, y )


type alias Coord units =
    ( Quantity Int units, Quantity Int units )
