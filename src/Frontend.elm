module Frontend exposing (app, init, update, updateFromBackend, view)

import Ascii
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Element
import Grid
import Html exposing (Html)
import Html.Attributes
import Keyboard
import Keyboard.Arrows
import Lamdera
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Pixels
import Point2d
import Quantity
import Task
import Types exposing (..)
import Url
import Vector2d
import WebGL exposing (Shader)
import WebGL.Settings
import WebGL.Texture exposing (Texture)


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , grid = Grid.empty
      , viewPoint = ( Quantity.zero, Quantity.zero )
      , cursorPoint = Nothing
      , texture = Nothing
      , pressedKeys = []
      , windowSize = ( Pixels.pixels 100, Pixels.pixels 100 )
      }
    , Cmd.batch
        [ WebGL.Texture.loadWith
            { magnify = WebGL.Texture.nearest
            , minify = WebGL.Texture.nearest
            , horizontalWrap = WebGL.Texture.clampToEdge
            , verticalWrap = WebGL.Texture.clampToEdge
            , flipY = False
            }
            Ascii.textureData
            |> Task.attempt TextureLoaded
        , Task.perform
            (\{ viewport } -> WindowResized ( round viewport.width |> Pixels.pixels, round viewport.height |> Pixels.pixels ))
            Browser.Dom.getViewport
        ]
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        TextureLoaded result ->
            case result of
                Ok texture ->
                    ( { model | texture = Just texture }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }
            , Cmd.none
            )

        Step time ->
            let
                ( x, y ) =
                    model.viewPoint

                newViewPoint =
                    ( if isDown Keyboard.ArrowLeft model /= isDown Keyboard.ArrowRight model then
                        if isDown Keyboard.ArrowLeft model then
                            Quantity.plus x (Pixels.pixels -10)

                        else
                            Quantity.plus x (Pixels.pixels 10)

                      else
                        x
                    , if isDown Keyboard.ArrowUp model /= isDown Keyboard.ArrowDown model then
                        if isDown Keyboard.ArrowUp model then
                            Quantity.plus y (Pixels.pixels 10)

                        else
                            Quantity.plus y (Pixels.pixels -10)

                      else
                        y
                    )
            in
            ( { model | viewPoint = newViewPoint }
            , Cmd.none
            )

        WindowResized windowSize ->
            ( { model | windowSize = windowSize }, Cmd.none )


isDown : Keyboard.Key -> { a | pressedKeys : List Keyboard.Key } -> Bool
isDown key model =
    List.any ((==) key) model.pressedKeys


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    ( model, Cmd.none )


view : FrontendModel -> Browser.Document msg
view model =
    { title = ""
    , body =
        [ Element.layout
            [ Element.width Element.fill, Element.height Element.fill ]
            (Element.html (canvasView model))
        ]
    }


canvasView : FrontendModel -> Html msg
canvasView model =
    let
        ( windowWidth, windowHeight ) =
            model.windowSize
    in
    WebGL.toHtmlWith
        [ WebGL.alpha False ]
        [ Html.Attributes.width <| Pixels.inPixels windowWidth
        , Html.Attributes.height <| Pixels.inPixels windowHeight
        ]
        (case model.texture of
            Just texture ->
                let
                    ( x, y ) =
                        model.viewPoint
                in
                [ WebGL.entityWith
                    [ WebGL.Settings.cullFace WebGL.Settings.back ]
                    vertexShader
                    fragmentShader
                    test
                    { view =
                        Mat4.makeScale3 (2 / toFloat (Pixels.inPixels windowWidth)) (-2 / toFloat (Pixels.inPixels windowHeight)) 1
                            |> Mat4.translate3 -(x |> Pixels.inPixels |> toFloat |> (+) 0.5) (y |> Pixels.inPixels |> toFloat |> (+) 0.5) 0
                    , texture = texture
                    }
                ]

            Nothing ->
                []
        )


subscriptions model =
    Sub.batch
        [ Sub.map KeyMsg Keyboard.subscriptions
        , if List.isEmpty model.pressedKeys then
            Sub.none

          else
            Browser.Events.onAnimationFrame Step
        , Browser.Events.onResize (\width height -> WindowResized ( Pixels.pixels width, Pixels.pixels height ))
        ]


test =
    Grid.mesh (List.range 0 255 |> List.map (Ascii.ascii >> Maybe.withDefault Ascii.default))


type alias Uniforms =
    { view : Mat4
    , texture : Texture
    }


vertexShader : Shader { position : Vec2, texturePosition : Vec2 } { u | view : Mat4 } { vcoord : Vec2 }
vertexShader =
    [glsl|

attribute vec2 position;
attribute vec2 texturePosition;
uniform mat4 view;
varying vec2 vcoord;

void main () {
  gl_Position = view * vec4(position, 0.0, 1.0);
  vcoord = texturePosition;
}

|]


fragmentShader : Shader {} { u | texture : Texture } { vcoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;
        void main () {
            gl_FragColor = texture2D(texture, vcoord);
        }
    |]



--view : FrontendModel -> Browser.Document msg
--view model =
--    { title = ""
--    , body =
--        [ Element.layout
--            [ Element.Font.family [ Element.Font.monospace ], Element.Font.size 16, Element.moveDown 2 ]
--            (Ascii.ascii
--                |> List.Extra.groupsOf 25
--                |> List.map
--                    (List.map
--                        (String.fromChar
--                            >> Element.text
--                            >> Element.el [ Element.width (Element.px 10), Element.height (Element.px 16) ]
--                        )
--                        >> Element.row []
--                    )
--                |> Element.column [ Element.spacing 2, Element.htmlAttribute <| Html.Attributes.style "white-space" "pre" ]
--            )
--        ]
--    }
