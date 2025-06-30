module Shaders exposing (colorToVec3, fragmentShader, lch2rgb, userColor, userPixelColor, vertexShader)

import Basics.Extra as Basics
import Bitwise
import Element
import Grid
import Image
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


vertexShader : Shader Grid.Vertex { u | view : Mat4, highlightedUser : Float, showColors : Float, highlightIntensity : Float, hyperlinkMin : Vec2, hyperlinkMax : Vec2 } { vcoord : Vec2, vcolor : Vec4, isHyperlink : Float }
vertexShader =
    [glsl|
attribute vec2 position;
attribute vec2 quadPosition;
attribute vec2 texturePosition;
attribute float userId;
uniform mat4 view;
uniform float highlightedUser;
uniform float showColors;
uniform float highlightIntensity;
uniform vec2 hyperlinkMin;
uniform vec2 hyperlinkMax;
varying vec2 vcoord;
varying vec4 vcolor;
varying float isHyperlink;

const vec3 n = vec3(0.95047, 1.0, 1.08883);
const float t0 = 4.0 / 29.0;
const float t1 = 6.0 / 29.0;
const float t2 = 3.0 * pow(t1, 2.0);
const float t3 = pow(t1, 3.0);
const mat3 coeffs = mat3(
    3.2404542, -0.969266, 0.0556434, // first column
    -1.5371385, 1.8760108, -0.2040259,
    -0.4985314, 0.041556, 1.0572252
);

vec3 lessThanForMix(vec3 l, vec3 r) {
    return sign(r - l)/2.0 + vec3(0.5);
}

vec3 lab2xyz( vec3 t ) {
    return mix(
        t2 * (t - t0),
        pow(t, vec3(3.0)),
        lessThanForMix(vec3(t1), t)
    );
}

vec3 xyz2rgb(vec3 r) {
    return mix(
        1.055 * pow(r, vec3(1.0 / 2.4)) - 0.055,
        12.92 * r,
        lessThanForMix(r,vec3(0.00304))
    );
}

vec3 lab2rgb(float lightness, float labA, float labB ) {
    vec3 startY = vec3((lightness + 16.0) / 116.0);
    vec3 lab = startY + vec3(
        (labA / 500.0),
        0,
        -(labB / 200.0)
    );
    vec3 xyz = lab2xyz(lab) * n;
    vec3 rgb = xyz2rgb(coeffs * xyz);
    return clamp(rgb,0.0,1.0);
}

vec3 lch2rgb(float luminance, float chroma, float hue) {
    float hueInRadians = radians(hue);
    return lab2rgb( luminance, cos(hueInRadians) * chroma, sin(hueInRadians) * chroma );
}

void main () {
    gl_Position = view * vec4(position, 0.0, 1.0);

    bool insideHyperlink =
        quadPosition.x > hyperlinkMin.x && quadPosition.x < hyperlinkMax.x &&
            quadPosition.y > hyperlinkMin.y && quadPosition.y < hyperlinkMax.y;

    vcoord = texturePosition + (insideHyperlink ? vec2(0.0, 0.5) : vec2(0.0, 0.0));
    isHyperlink = float(insideHyperlink);

    float userIdFloat = userId + 125.0;
    float luminance = mod(userIdFloat * 0.5219, 1.0) * 20.0 + 75.0;
    float chroma = mod(userIdFloat * 0.4237, 1.0) * 60.0 + 0.0;
    float hue = userIdFloat * 101.93;
    vec3 rgbColor = lch2rgb(userId == highlightedUser ? luminance + highlightIntensity : luminance, chroma, hue);

    vcolor = userId != -9.0 && showColors == 1.0
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
            |> fractionalModBy 1
            |> (*) 20
            |> (+) 75
    , chroma = userIdFloat * 0.4237 |> fractionalModBy 1 |> (*) 60 |> (+) 0
    , hue = userIdFloat * 101.93
    }


{-| Perform [modular arithmetic](https://en.wikipedia.org/wiki/Modular_arithmetic)
involving floating point numbers.

The sign of the result is the same as the sign of the `modulus`
in `fractionalModBy modulus x`.

    fractionalModBy 2.5 5 --> 0

    fractionalModBy 2 4.5 == 0.5

    fractionalModBy 2 -4.5 == 1.5

    fractionalModBy -2 4.5 == -1.5

Copied from here <https://github.com/elm-community/basics-extra/blob/8081dfb51115a0fe3c483f9bcf1e2428d7e80aec/src/Basics/Extra.elm#L203C1-L220C48>

-}
fractionalModBy : Float -> Float -> Float
fractionalModBy modulus x =
    x - modulus * toFloat (floor (x / modulus))


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


userPixelColor userId =
    let
        { red, green, blue } =
            userColor userId |> lch2rgb |> Element.toRgb
    in
    (Bitwise.shiftLeftBy 24 <| round (255 * red)) + (Bitwise.shiftLeftBy 16 <| round (255 * green)) + (Bitwise.shiftLeftBy 8 <| round (255 * blue)) + 255


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


fragmentShader : Shader {} { u | texture : Texture } { vcoord : Vec2, vcolor : Vec4, isHyperlink : Float }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;
        varying vec4 vcolor;
        varying float isHyperlink;

        void main () {
            vec4 textureColor = texture2D(texture, vcoord);

            vec4 textColor =
                isHyperlink == 1.0
                    ? vec4(0.0, 0.0, 1.0, 1.0)
                    : vec4(vcolor.xyz * 0.3,1.0);
            vec4 backColor = vcolor;

            gl_FragColor =
                float(textureColor.x == 1.0) * textColor
                    + float(textureColor.x == 0.0) * backColor;
        }
    |]
