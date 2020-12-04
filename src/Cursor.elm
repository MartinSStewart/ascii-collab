module Cursor exposing (Cursor, bounds, draw, fragmentShader, mesh, moveCursor, newLine, position, selection, setCursor, toMesh, updateMesh, vertexShader)

import Ascii
import Bounds exposing (Bounds)
import Element
import Helper exposing (Coord)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Quantity exposing (Quantity(..))
import Shaders
import Units
import WebGL exposing (Shader)
import WebGL.Settings


type Cursor
    = Cursor
        { position : Coord Units.AsciiUnit
        , startingColumn : Quantity Int Units.AsciiUnit
        , size : Coord Units.AsciiUnit
        }


moveCursor : Bool -> ( Quantity Int Units.AsciiUnit, Quantity Int Units.AsciiUnit ) -> Cursor -> Cursor
moveCursor isShiftDown offset (Cursor cursor) =
    if isShiftDown then
        Cursor
            { cursor
                | position = Helper.addTuple offset cursor.position
                , size = cursor.size |> Helper.minusTuple offset
            }

    else
        Cursor
            { cursor
                | position = Helper.addTuple offset cursor.position
                , size = ( Units.asciiUnit 0, Units.asciiUnit 0 )
            }


newLine : Cursor -> Cursor
newLine (Cursor cursor) =
    Cursor
        { position = ( cursor.startingColumn, Tuple.second cursor.position |> Quantity.plus (Units.asciiUnit 1) )
        , startingColumn = cursor.startingColumn
        , size = ( Units.asciiUnit 0, Units.asciiUnit 0 )
        }


setCursor : ( Quantity Int Units.AsciiUnit, Quantity Int Units.AsciiUnit ) -> Cursor
setCursor setPosition =
    Cursor
        { position = setPosition
        , startingColumn = Tuple.first setPosition
        , size = ( Units.asciiUnit 0, Units.asciiUnit 0 )
        }


position : Cursor -> Coord Units.AsciiUnit
position (Cursor cursor) =
    cursor.position


mesh : Int -> Float -> Float -> Float -> Float -> ( List { position : Vec2 }, List ( Int, Int, Int ) )
mesh indexOffset x y w h =
    ( [ { position = Math.Vector2.vec2 x y }
      , { position = Math.Vector2.vec2 (x + w) y }
      , { position = Math.Vector2.vec2 (x + w) (y + h) }
      , { position = Math.Vector2.vec2 x (y + h) }
      ]
    , [ ( indexOffset, indexOffset + 3, indexOffset + 1 ), ( indexOffset + 2, indexOffset + 1, indexOffset + 3 ) ]
    )


toMesh : Cursor -> WebGL.Mesh { position : Vec2 }
toMesh cursor =
    let
        thickness =
            3

        ( cw, ch ) =
            size cursor
                |> Helper.toRawCoord
                |> Tuple.mapBoth (abs >> (+) 1) (abs >> (+) 1)

        ( cw_, ch_ ) =
            size cursor |> Helper.toRawCoord

        ( w, h ) =
            Helper.toRawCoord Ascii.size

        ( v0, i0 ) =
            mesh 0
                (if cw_ > 0 then
                    0

                 else
                    toFloat <| abs cw_ * w
                )
                (if ch_ > 0 then
                    0

                 else
                    toFloat <| abs ch_ * h
                )
                (toFloat w)
                (toFloat h)

        ( v1, i1 ) =
            mesh 4 0 0 thickness (toFloat <| ch * h)

        ( v2, i2 ) =
            mesh 8 (toFloat <| cw * w - thickness) 0 thickness (toFloat <| ch * h)

        ( v3, i3 ) =
            mesh 12 0 0 (toFloat <| cw * w) thickness

        ( v4, i4 ) =
            mesh 16 0 (toFloat <| ch * h - thickness) (toFloat <| cw * w) thickness
    in
    WebGL.indexedTriangles
        (v0 ++ v1 ++ v2 ++ v3 ++ v4)
        (i0 ++ i1 ++ i2 ++ i3 ++ i4)


updateMesh :
    { a | cursor : Cursor, cursorMesh : WebGL.Mesh { position : Vec2 } }
    -> { a | cursor : Cursor, cursorMesh : WebGL.Mesh { position : Vec2 } }
    -> { a | cursor : Cursor, cursorMesh : WebGL.Mesh { position : Vec2 } }
updateMesh oldModel newModel =
    if size oldModel.cursor == size newModel.cursor then
        newModel

    else
        { newModel | cursorMesh = toMesh newModel.cursor }


size : Cursor -> Coord Units.AsciiUnit
size (Cursor cursor) =
    cursor.size


selection : Coord Units.AsciiUnit -> Coord Units.AsciiUnit -> Cursor
selection start end =
    { position = end
    , size = Helper.minusTuple end start
    , startingColumn = Quantity.min (Tuple.first start) (Tuple.first end)
    }
        |> Cursor


bounds : Cursor -> Bounds Units.AsciiUnit
bounds (Cursor cursor) =
    let
        pos0 =
            cursor.position

        pos1 =
            Helper.addTuple cursor.position cursor.size
    in
    Bounds.bounds
        (Helper.minTuple pos0 pos1)
        (Helper.maxTuple pos0 pos1 |> Helper.addTuple ( Units.asciiUnit 1, Units.asciiUnit 1 ))


draw : Mat4 -> Element.Color -> { a | cursor : Cursor, cursorMesh : WebGL.Mesh { position : Vec2 } } -> WebGL.Entity
draw viewMatrix color model =
    let
        bounds_ =
            bounds model.cursor
    in
    WebGL.entityWith
        [ WebGL.Settings.cullFace WebGL.Settings.back ]
        vertexShader
        fragmentShader
        model.cursorMesh
        { view = viewMatrix
        , offset = bounds_.min |> Units.asciiToWorld |> Helper.coordToVec
        , color = Shaders.colorToVec3 color
        }


vertexShader : Shader { position : Vec2 } { u | view : Mat4, offset : Vec2 } {}
vertexShader =
    [glsl|

attribute vec2 position;
uniform mat4 view;
uniform vec2 offset;

void main () {
  gl_Position = view * vec4(position + offset, 0.0, 1.0);
}

|]


fragmentShader : Shader {} { u | color : Vec3 } {}
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        void main () {
            gl_FragColor = vec4(color, 1.0);
        }
    |]
