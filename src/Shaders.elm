module Shaders exposing (fragmentShader, userColor, vertexShader)

import Angle
import Basics.Extra as Basics
import ColorHelper
import Element
import Grid
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector4 exposing (Vec4)
import User exposing (UserId)
import WebGL exposing (Shader)
import WebGL.Texture exposing (Texture)


vertexShader : Shader Grid.Vertex { u | view : Mat4, highlightedUser : Float, showColors : Float } { vcoord : Vec2, vcolor : Vec4 }
vertexShader =
    [glsl|

attribute vec2 position;
attribute vec2 texturePosition;
attribute float userId;
uniform mat4 view;
uniform float highlightedUser;
uniform float showColors;
varying vec2 vcoord;
varying vec4 vcolor;

vec3 lab2xyz( vec3 c ) {
    float fy = ( c.x + 16.0 ) / 116.0;
    float fx = c.y / 500.0 + fy;
    float fz = fy - c.z / 200.0;
    return vec3(
         95.047 * (( fx > 0.206897 ) ? fx * fx * fx : ( fx - 16.0 / 116.0 ) / 7.787),
        100.000 * (( fy > 0.206897 ) ? fy * fy * fy : ( fy - 16.0 / 116.0 ) / 7.787),
        108.883 * (( fz > 0.206897 ) ? fz * fz * fz : ( fz - 16.0 / 116.0 ) / 7.787)
    );
}

vec3 xyz2rgb( vec3 c ) {
	const mat3 mat = mat3(
        3.2406, -1.5372, -0.4986,
        -0.9689, 1.8758, 0.0415,
        0.0557, -0.2040, 1.0570
	);
    vec3 v = ((c / 100.0) * mat);
    vec3 r;
    r.x = ( v.r > 0.0031308 ) ? (( 1.055 * pow( v.r, ( 1.0 / 2.4 ))) - 0.055 ) : 12.92 * v.r;
    r.y = ( v.g > 0.0031308 ) ? (( 1.055 * pow( v.g, ( 1.0 / 2.4 ))) - 0.055 ) : 12.92 * v.g;
    r.z = ( v.b > 0.0031308 ) ? (( 1.055 * pow( v.b, ( 1.0 / 2.4 ))) - 0.055 ) : 12.92 * v.b;
    return r;
}

vec3 lab2rgb( vec3 c ) {
    return xyz2rgb( lab2xyz( vec3(100.0 * c.x, 2.0 * 127.0 * (c.y - 0.5), 2.0 * 127.0 * (c.z - 0.5)) ) );
}

vec3 lch2rgb( vec3 c ) {
    float hueInDegrees = 3.14159265 * c.z / 360.0;
    return lab2rgb(vec3( c.x, cos(hueInDegrees) * c.y, sin(hueInDegrees) * c.y ));
}

void main () {
    gl_Position = view * vec4(position, 0.0, 1.0);
    vcoord = texturePosition;

    float userIdFloat = userId + 124.0;
    float luminance = mod(userIdFloat * 0.5219, 1.0) * 50.0 + 45.0;
    float chroma = mod(userIdFloat * 0.4237, 1.0) * 110.0 + 20.0;
    float hue = userIdFloat * 101.93;
    vec3 rgbColor = lch2rgb(vec3(luminance, chroma, hue));

    vcolor = float(userId != -1.0 && showColors == 1.0) * vec4(rgbColor, 1.0);
}

|]


userColor : UserId -> Element.Color
userColor userId =
    let
        userIdFloat =
            toFloat (User.rawId userId + 124)
    in
    lch2lab
        { luminance = userIdFloat * 0.5219 |> Basics.fractionalModBy 1 |> (*) 50 |> (+) 45
        , chroma = userIdFloat * 0.4237 |> Basics.fractionalModBy 1 |> (*) 110 |> (+) 20
        , hue = userIdFloat * 101.93
        }
        |> lab2rgb


lab2rgb { lightness, labA, labB } =
    let
        startY =
            (lightness + 16) / 116

        y =
            startY |> lab2xyz |> (*) yn

        x =
            startY + (labA / 500) |> lab2xyz |> (*) xn

        z =
            startY - (labB / 200) |> lab2xyz |> (*) zn

        r =
            (3.2404542 * x) + (-1.5371385 * y) + (-0.4985314 * z) |> xyz2rgb

        g =
            (-0.969266 * x) + (1.8760108 * y) + (0.041556 * z) |> xyz2rgb

        b =
            (0.0556434 * x) + (-0.2040259 * y) + (1.0572252 * z) |> xyz2rgb
    in
    Element.rgb (clamp 0 1 r) (clamp 0 1 g) (clamp 0 1 b)


lab2xyz : Float -> Float
lab2xyz t =
    if t > t1 then
        t ^ 3

    else
        t2 * (t - t0)


xyz2rgb : Float -> Float
xyz2rgb r =
    if r <= 0.00304 then
        12.92 * r

    else
        1.055 * r ^ (1 / 2.4) - 0.055


kn : Float
kn =
    18


xn : Float
xn =
    0.95047


yn : Float
yn =
    1


zn : Float
zn =
    1.08883


t0 : Float
t0 =
    4 / 29


t1 : Float
t1 =
    6 / 29


t2 : Float
t2 =
    3 * t1 ^ 2


t3 : Float
t3 =
    t1 ^ 3


lch2lab { luminance, chroma, hue } =
    let
        hueInDegrees =
            if isNaN hue then
                0

            else
                degrees hue
    in
    { lightness = luminance, labA = cos hueInDegrees * chroma, labB = sin hueInDegrees * chroma }


fragmentShader : Shader {} { u | texture : Texture } { vcoord : Vec2, vcolor : Vec4 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;
        varying vec4 vcolor;
        void main () {
            vec4 textureColor = texture2D(texture, vcoord);

            vec4 textColor = vec4(vcolor.xyz * 0.4,1.0);
            vec4 backColor = vcolor;

            gl_FragColor =
                float(textureColor.x == 1.0) * textColor
                    + float(textureColor.x == 0.0) * backColor;
        }
    |]
