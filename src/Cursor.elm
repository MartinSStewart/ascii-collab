module Cursor exposing (Cursor, fragmentShader, mesh, moveCursor, newLine, setCursor, vertexShader)

import Ascii
import Helper
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Quantity exposing (Quantity(..))
import Units
import WebGL exposing (Shader)


type alias Cursor =
    { position : ( Quantity Int Units.AsciiUnit, Quantity Int Units.AsciiUnit )
    , startingColumn : Quantity Int Units.AsciiUnit
    }


moveCursor : ( Quantity Int Units.AsciiUnit, Quantity Int Units.AsciiUnit ) -> Cursor -> Cursor
moveCursor offset { position, startingColumn } =
    { position = Helper.addTuple offset position
    , startingColumn = startingColumn
    }


newLine : Cursor -> Cursor
newLine { position, startingColumn } =
    { position = ( startingColumn, Tuple.second position |> Quantity.plus (Units.asciiUnit 1) ), startingColumn = startingColumn }


setCursor : ( Quantity Int Units.AsciiUnit, Quantity Int Units.AsciiUnit ) -> Cursor
setCursor position =
    { position = position
    , startingColumn = Tuple.first position
    }


mesh : WebGL.Mesh { position : Vec2 }
mesh =
    let
        ( Quantity w, Quantity h ) =
            Ascii.size
    in
    [ { position = Math.Vector2.vec2 0 0 }
    , { position = Math.Vector2.vec2 w 0 }
    , { position = Math.Vector2.vec2 w h }
    , { position = Math.Vector2.vec2 0 h }
    ]
        |> WebGL.triangleFan


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
