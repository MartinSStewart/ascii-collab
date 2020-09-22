module Shaders exposing (colorToVec3, fragmentShader, lch2rgb, userColor, vertexShader)

import Basics.Extra as Basics
import Element
import Grid
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3
import Math.Vector4 exposing (Vec4)
import User exposing (UserId)
import WebGL exposing (Shader)
import WebGL.Texture exposing (Texture)


colorToVec3 : Element.Color -> Math.Vector3.Vec3
colorToVec3 color =
    let
        { red, green, blue } =
            Element.toRgb color
    in
    Math.Vector3.vec3 red green blue


vertexShader : Shader Grid.Vertex { u | view : Mat4, highlightedUser : Float, showColors : Float, highlightIntensity : Float } { vcoord : Vec2, vcolor : Vec4 }
vertexShader =
    [glsl|
attribute vec2 position;
attribute vec2 texturePosition;
attribute float userId;
uniform mat4 view;
uniform float highlightedUser;
uniform float showColors;
uniform float highlightIntensity;
varying vec2 vcoord;
varying vec4 vcolor;

const float kn = 18.0;
const float xn = 0.95047;
const float zn = 1.08883;
const float t0 = 4.0 / 29.0;
const float t1 = 6.0 / 29.0;
const float t2 = 3.0 * pow(t1, 2.0);
const float t3 = pow(t1, 3.0);

float lab2xyz( float t ) {
    return t > t1 ? pow(t, 3.0) : t2 * (t - t0);
}

float xyz2rgb ( float r ) {
    return r <= 0.00304 ? (12.92 * r) : (1.055 * pow(r, 1.0 / 2.4) - 0.055);
}

vec3 lab2rgb(float lightness, float labA, float labB ) {
    float startY = (lightness + 16.0) / 116.0;
    float y = lab2xyz(startY);
    float x = lab2xyz(startY + (labA / 500.0)) * xn;
    float z = lab2xyz(startY - (labB / 200.0)) * zn;
    float r = xyz2rgb((3.2404542 * x) + (-1.5371385 * y) + (-0.4985314 * z));
    float g = xyz2rgb((-0.969266 * x) + (1.8760108 * y) + (0.041556 * z));
    float b = xyz2rgb((0.0556434 * x) + (-0.2040259 * y) + (1.0572252 * z));
    return vec3(clamp(r,0.0,1.0), clamp(g,0.0,1.0), clamp(b,0.0,1.0));
}

vec3 lch2rgb( float luminance, float chroma, float hue ) {
    float hueInRadians = hue * (3.14159265 / 180.0);
    return lab2rgb( luminance, cos(hueInRadians) * chroma, sin(hueInRadians) * chroma );
}

void main () {
    gl_Position = view * vec4(position, 0.0, 1.0);
    vcoord = texturePosition;

    float userIdFloat = userId + 125.0;
    float luminance = mod(userIdFloat * 0.5219, 1.0) * 20.0 + 75.0;
    float chroma = mod(userIdFloat * 0.4237, 1.0) * 60.0 + 0.0;
    float hue = userIdFloat * 101.93;
    vec3 rgbColor = lch2rgb(userId == highlightedUser ? luminance + highlightIntensity : luminance, chroma, hue);

    vcolor = userId != -1.0 && showColors == 1.0
        ? vec4(rgbColor, 1.0)
        : userId == highlightedUser
            ? vec4(rgbColor, 1.0)
            : vec4(0.0,0.0,0.0,0.0);
}

|]


userColor : UserId -> { luminance : Float, chroma : Float, hue : Float }
userColor userId =
    let
        userIdFloat =
            toFloat (User.rawId userId + 125)
    in
    { luminance =
        (userIdFloat * 0.5219)
            |> Basics.fractionalModBy 1
            |> (*) 20
            |> (+) 75
    , chroma = userIdFloat * 0.4237 |> Basics.fractionalModBy 1 |> (*) 60 |> (+) 0
    , hue = userIdFloat * 101.93
    }


lab2rgb : { lightness : Float, labA : Float, labB : Float } -> Element.Color
lab2rgb { lightness, labA, labB } =
    let
        startY =
            (lightness + 16) / 116

        y =
            lab2xyz startY

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


lch2rgb : { luminance : Float, chroma : Float, hue : Float } -> Element.Color
lch2rgb { luminance, chroma, hue } =
    let
        hueInRadians =
            if isNaN hue then
                0

            else
                degrees hue
    in
    lab2rgb { lightness = luminance, labA = cos hueInRadians * chroma, labB = sin hueInRadians * chroma }


fragmentShader : Shader {} { u | texture : Texture } { vcoord : Vec2, vcolor : Vec4 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;
        varying vec4 vcolor;
        void main () {
            vec4 textureColor = texture2D(texture, vcoord);

            vec4 textColor = vec4(vcolor.xyz * 0.3,1.0);
            vec4 backColor = vcolor;

            gl_FragColor =
                float(textureColor.x == 1.0) * textColor
                    + float(textureColor.x == 0.0) * backColor;
        }
    |]
