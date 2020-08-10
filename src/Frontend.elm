port module Frontend exposing (app, init, update, updateFromBackend, view)

import Ascii
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Cursor
import Element
import Grid
import Helper
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Keyboard
import Keyboard.Arrows
import Lamdera
import List.Extra as List
import List.Nonempty
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Pixels exposing (Pixels)
import Point2d
import Quantity exposing (Quantity)
import Task
import Types exposing (..)
import Units
import Url
import User exposing (UserId(..))
import Vector2d
import WebGL exposing (Shader)
import WebGL.Settings
import WebGL.Texture exposing (Texture)


port martinsstewart_elm_device_pixel_ratio_from_js : (Float -> msg) -> Sub msg


port martinsstewart_elm_device_pixel_ratio_to_js : () -> Cmd msg


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
      , cursor = Just { position = ( Units.asciiUnit 0, Units.asciiUnit 0 ), startingColumn = Units.asciiUnit 0 }
      , texture = Nothing
      , pressedKeys = []
      , windowSize = ( Pixels.pixels 100, Pixels.pixels 100 )
      , devicePixelRatio = 1
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

        KeyDown rawKey ->
            ( case ( model.cursor, Keyboard.anyKeyOriginal rawKey ) of
                --( Just cursor, Just Keyboard.Enter ) ->
                --    { model | cursor = Cursor.newLine cursor |> Just }
                _ ->
                    model
            , Cmd.none
            )

        Step _ ->
            let
                newViewPoint =
                    ( if isDown Keyboard.ArrowLeft model /= isDown Keyboard.ArrowRight model then
                        if isDown Keyboard.ArrowLeft model then
                            Units.worldUnit -10

                        else
                            Units.worldUnit 10

                      else
                        Quantity.zero
                    , if isDown Keyboard.ArrowUp model /= isDown Keyboard.ArrowDown model then
                        if isDown Keyboard.ArrowUp model then
                            Units.worldUnit 10

                        else
                            Units.worldUnit -10

                      else
                        Quantity.zero
                    )
            in
            ( { model | viewPoint = Helper.addTuple model.viewPoint newViewPoint }
            , Cmd.none
            )

        WindowResized windowSize ->
            ( { model | windowSize = windowSize }, martinsstewart_elm_device_pixel_ratio_to_js () )

        GotDevicePixelRatio devicePixelRatio ->
            ( { model | devicePixelRatio = devicePixelRatio }, Cmd.none )

        UserTyped text ->
            ( case model.cursor of
                Just cursor ->
                    if text == "\n" || text == "\n\u{000D}" then
                        { model | cursor = Cursor.newLine cursor |> Just }

                    else
                        String.filter ((/=) '\u{000D}') text
                            |> String.split "\n"
                            |> List.Nonempty.fromList
                            |> Maybe.map (List.Nonempty.map (String.toList >> List.map (Ascii.charToAscii >> Maybe.withDefault Ascii.default)))
                            |> Maybe.map
                                (\lines ->
                                    { model
                                        | grid =
                                            Grid.addChange (UserId 0) cursor.position lines model.grid
                                        , cursor =
                                            Cursor.moveCursor
                                                ( Units.asciiUnit (List.Nonempty.last lines |> List.length)
                                                , Units.asciiUnit (List.Nonempty.length lines - 1)
                                                )
                                                cursor
                                                |> Just
                                    }
                                )
                            |> Maybe.withDefault model

                _ ->
                    model
            , Cmd.none
            )

        MouseDown mousePosition ->
            ( model, Cmd.none )


isDown : Keyboard.Key -> { a | pressedKeys : List Keyboard.Key } -> Bool
isDown key model =
    List.any ((==) key) model.pressedKeys


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    ( model, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Ascii Art"
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.clip
            , Element.inFront <|
                Element.html <|
                    Html.textarea
                        [ Html.Events.onInput UserTyped
                        , Html.Attributes.value ""
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "height" "100%"
                        , Html.Attributes.style "resize" "none"
                        , Html.Attributes.style "background-color" "transparent"
                        , Html.Attributes.style "border-width" "0px"
                        ]
                        []
            ]
            (Element.html (canvasView model))
        ]
    }


findPixelPerfectSize : FrontendModel -> { canvasSize : ( Int, Int ), actualCanvasSize : ( Int, Int ) }
findPixelPerfectSize frontendModel =
    let
        findValue : Quantity Int Pixels -> ( Int, Int )
        findValue value =
            List.range 0 9
                |> List.map ((+) (Pixels.inPixels value))
                |> List.find
                    (\v ->
                        let
                            a =
                                toFloat v * frontendModel.devicePixelRatio
                        in
                        a == toFloat (round a) && modBy 2 (round a) == 0
                    )
                |> Maybe.map (\v -> ( v, toFloat v * frontendModel.devicePixelRatio |> round ))
                |> Maybe.withDefault ( Pixels.inPixels value, toFloat (Pixels.inPixels value) * frontendModel.devicePixelRatio |> round )

        ( w, actualW ) =
            findValue (Tuple.first frontendModel.windowSize)

        ( h, actualH ) =
            findValue (Tuple.second frontendModel.windowSize)
    in
    { canvasSize = ( w, h ), actualCanvasSize = ( actualW, actualH ) }


canvasView : FrontendModel -> Html FrontendMsg
canvasView model =
    let
        ( windowWidth, windowHeight ) =
            actualCanvasSize

        ( cssWindowWidth, cssWindowHeight ) =
            canvasSize

        { canvasSize, actualCanvasSize } =
            findPixelPerfectSize model

        ( x, y ) =
            model.viewPoint

        viewMatrix =
            Mat4.makeScale3 (2 / toFloat windowWidth) (-2 / toFloat windowHeight) 1
                |> Mat4.translate3
                    (x |> Units.inWorldUnits |> toFloat |> negate)
                    (y |> Units.inWorldUnits |> toFloat)
                    0
    in
    WebGL.toHtmlWith
        [ WebGL.alpha False, WebGL.antialias ]
        [ Html.Attributes.width windowWidth
        , Html.Attributes.height windowHeight
        , Html.Attributes.style "width" (String.fromInt cssWindowWidth ++ "px")
        , Html.Attributes.style "height" (String.fromInt cssWindowHeight ++ "px")
        ]
        ((case model.cursor of
            Just cursor ->
                [ WebGL.entity
                    Cursor.vertexShader
                    Cursor.fragmentShader
                    Cursor.mesh
                    { view = viewMatrix
                    , offset = Units.asciiToWorld cursor.position |> Helper.coordToVec
                    , color = Math.Vector3.vec3 1 1 0
                    }
                ]

            Nothing ->
                []
         )
            ++ (case model.texture of
                    Just texture ->
                        Grid.meshes model.grid
                            |> List.map
                                (\mesh ->
                                    WebGL.entityWith
                                        [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                        vertexShader
                                        fragmentShader
                                        mesh
                                        { view = viewMatrix
                                        , texture = texture
                                        , color = Math.Vector3.vec3 0 0 0
                                        }
                                )

                    Nothing ->
                        []
               )
        )


subscriptions model =
    Sub.batch
        [ Sub.map KeyMsg Keyboard.subscriptions
        , Keyboard.downs KeyDown
        , if Keyboard.Arrows.arrowsDirection model.pressedKeys == Keyboard.Arrows.NoDirection then
            Sub.none

          else
            Browser.Events.onAnimationFrame Step
        , Browser.Events.onResize (\width height -> WindowResized ( Pixels.pixels width, Pixels.pixels height ))
        , martinsstewart_elm_device_pixel_ratio_from_js GotDevicePixelRatio
        , Browser.Events.onMouseDown
            (Json.Decode.value
                |> Json.Decode.map
                    (\a ->
                        let
                            _ =
                                Debug.log "asfd" a
                        in
                        MouseDown ( Quantity.zero, Quantity.zero )
                    )
            )
        ]


type alias Uniforms =
    { view : Mat4
    , texture : Texture
    , color : Vec3
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


fragmentShader : Shader {} { u | texture : Texture, color : Vec3 } { vcoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        uniform vec3 color;
        varying vec2 vcoord;
        void main () {
            vec4 textureColor = texture2D(texture, vcoord);
            gl_FragColor = vec4(color.x * textureColor.x, color.y * textureColor.x, color.z * textureColor.x, textureColor.x);
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
