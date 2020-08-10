port module Frontend exposing (app, init, update, updateFromBackend, view)

import Ascii
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Cursor
import Element
import Grid exposing (Grid, ServerGrid)
import Helper
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse
import Keyboard
import Keyboard.Arrows
import Lamdera
import List.Extra as List
import List.Nonempty
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))
import Task
import Types exposing (..)
import Units exposing (ScreenCoordinate, WorldCoordinate, WorldPixel)
import Url
import User exposing (UserId(..))
import Vector2d exposing (Vector2d)
import WebGL exposing (Shader)
import WebGL.Settings
import WebGL.Settings.Blend as Blend
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


loadedInit : Browser.Navigation.Key -> ServerGrid -> UserId -> ( FrontendModel, Cmd FrontendMsg )
loadedInit key grid userId =
    ( Loaded
        { key = key
        , grid = Grid.toGrid grid
        , viewPoint = Point2d.origin
        , cursor = { position = ( Units.asciiUnit 0, Units.asciiUnit 0 ), startingColumn = Units.asciiUnit 0 }
        , texture = Nothing
        , pressedKeys = []
        , windowSize = ( Pixels.pixels 100, Pixels.pixels 100 )
        , devicePixelRatio = Quantity.per Pixels.pixel (Units.worldUnit 1)
        , mouseState = MouseLeftUp
        , userId = userId
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
        , Browser.Dom.focus "textareaId" |> Task.attempt (\_ -> NoOpFrontendMsg)
        ]
    )


init : Url.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ key =
    ( Loading { key = key }
    , Lamdera.sendToBackend RequestData
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case model of
        Loading _ ->
            ( model, Cmd.none )

        Loaded frontendLoaded ->
            updateLoaded msg frontendLoaded |> Tuple.mapFirst Loaded


updateLoaded : FrontendMsg -> FrontendLoaded -> ( FrontendLoaded, Cmd FrontendMsg )
updateLoaded msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Browser.Navigation.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
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
            ( case Keyboard.anyKeyOriginal rawKey of
                Just Keyboard.ArrowLeft ->
                    { model
                        | cursor =
                            Cursor.setCursor
                                (Helper.addTuple model.cursor.position ( Units.asciiUnit -1, Units.asciiUnit 0 ))
                    }

                Just Keyboard.ArrowRight ->
                    { model
                        | cursor =
                            Cursor.setCursor
                                (Helper.addTuple model.cursor.position ( Units.asciiUnit 1, Units.asciiUnit 0 ))
                    }

                Just Keyboard.ArrowUp ->
                    { model
                        | cursor =
                            Cursor.setCursor
                                (Helper.addTuple model.cursor.position ( Units.asciiUnit 0, Units.asciiUnit -1 ))
                    }

                Just Keyboard.ArrowDown ->
                    { model
                        | cursor =
                            Cursor.setCursor
                                (Helper.addTuple model.cursor.position ( Units.asciiUnit 0, Units.asciiUnit 1 ))
                    }

                Just Keyboard.Backspace ->
                    let
                        newCursor =
                            Cursor.setCursor
                                (Helper.addTuple model.cursor.position ( Units.asciiUnit -1, Units.asciiUnit 0 ))
                    in
                    { model
                        | cursor = newCursor
                        , grid = Grid.addChange model.userId newCursor.position (List.Nonempty.fromElement [ Ascii.default ]) model.grid
                    }

                _ ->
                    model
            , Cmd.none
            )

        Step _ ->
            ( model, Cmd.none )

        WindowResized windowSize ->
            ( { model | windowSize = windowSize }, martinsstewart_elm_device_pixel_ratio_to_js () )

        GotDevicePixelRatio devicePixelRatio ->
            ( { model | devicePixelRatio = devicePixelRatio }, Cmd.none )

        UserTyped text ->
            ( if text == "\n" || text == "\n\u{000D}" then
                { model | cursor = Cursor.newLine model.cursor }

              else
                String.filter ((/=) '\u{000D}') text
                    |> String.split "\n"
                    |> List.Nonempty.fromList
                    |> Maybe.map (List.Nonempty.map (String.toList >> List.map (Ascii.charToAscii >> Maybe.withDefault Ascii.default)))
                    |> Maybe.map
                        (\lines ->
                            { model
                                | grid =
                                    Grid.addChange model.userId model.cursor.position lines model.grid
                                , cursor =
                                    Cursor.moveCursor
                                        ( Units.asciiUnit (List.Nonempty.last lines |> List.length)
                                        , Units.asciiUnit (List.Nonempty.length lines - 1)
                                        )
                                        model.cursor
                            }
                        )
                    |> Maybe.withDefault model
            , Cmd.none
            )

        MouseDown mousePosition ->
            ( { model | mouseState = MouseLeftDown { start = mousePosition, current = mousePosition } }
            , Browser.Dom.focus "textareaId" |> Task.attempt (\_ -> NoOpFrontendMsg)
            )

        MouseUp mousePosition ->
            case model.mouseState of
                MouseLeftDown { start } ->
                    ( { model
                        | mouseState = MouseLeftUp
                        , viewPoint = offsetViewPoint model start mousePosition
                        , cursor =
                            if Vector2d.from start mousePosition |> Vector2d.length |> Quantity.lessThan (Pixels.pixels 15) then
                                let
                                    ( w, h ) =
                                        model.windowSize
                                in
                                mousePosition
                                    |> Point2d.translateBy
                                        (Vector2d.xy (Quantity.toFloatQuantity w) (Quantity.toFloatQuantity h)
                                            |> Vector2d.scaleBy -0.5
                                        )
                                    |> Point2d.at model.devicePixelRatio
                                    |> Point2d.placeIn (Units.screenFrame model.viewPoint)
                                    |> Units.worldToAscii
                                    |> Cursor.setCursor

                            else
                                model.cursor
                      }
                    , Cmd.none
                    )

                MouseLeftUp ->
                    ( model, Cmd.none )

        MouseMove mousePosition ->
            case model.mouseState of
                MouseLeftDown { start } ->
                    ( { model | mouseState = MouseLeftDown { start = start, current = mousePosition } }, Cmd.none )

                MouseLeftUp ->
                    ( model, Cmd.none )


offsetViewPoint : FrontendLoaded -> Point2d Pixels ScreenCoordinate -> Point2d Pixels ScreenCoordinate -> Point2d WorldPixel WorldCoordinate
offsetViewPoint { windowSize, viewPoint, devicePixelRatio } mouseStart mouseCurrent =
    let
        delta : Vector2d WorldPixel WorldCoordinate
        delta =
            Vector2d.from mouseCurrent mouseStart |> Vector2d.at devicePixelRatio |> Vector2d.placeIn (Units.screenFrame viewPoint)
    in
    Point2d.translateBy delta viewPoint


actualViewPoint : FrontendLoaded -> Point2d WorldPixel WorldCoordinate
actualViewPoint model =
    case model.mouseState of
        MouseLeftUp ->
            model.viewPoint

        MouseLeftDown { start, current } ->
            offsetViewPoint model start current


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case ( model, msg ) of
        ( _, NoOpToFrontend ) ->
            ( model, Cmd.none )

        ( Loading { key }, LoadingData { grid, userId } ) ->
            loadedInit key grid userId

        ( Loaded loaded, GridChangeBroadcast record ) ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Ascii Art"
    , body =
        [ case model of
            Loading _ ->
                Element.layout
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    (Element.text "Loading")

            Loaded loadedModel ->
                Element.layout
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.clip
                    , Element.inFront <|
                        Element.html <|
                            Html.textarea
                                ([ Html.Events.onInput UserTyped
                                 , Html.Attributes.value ""
                                 , Html.Attributes.style "width" "100%"
                                 , Html.Attributes.style "height" "100%"
                                 , Html.Attributes.style "resize" "none"
                                 , Html.Attributes.style "background-color" "transparent"
                                 , Html.Attributes.style "border-width" "0px"
                                 , Html.Attributes.id "textareaId"
                                 , Html.Events.Extra.Mouse.onDown
                                    (\{ clientPos } ->
                                        MouseDown (Point2d.pixels (Tuple.first clientPos) (Tuple.second clientPos))
                                    )
                                 , Html.Events.Extra.Mouse.onUp
                                    (\{ clientPos } ->
                                        MouseUp (Point2d.pixels (Tuple.first clientPos) (Tuple.second clientPos))
                                    )
                                 ]
                                    ++ (case loadedModel.mouseState of
                                            MouseLeftDown _ ->
                                                [ Html.Events.Extra.Mouse.onMove
                                                    (\{ clientPos } ->
                                                        MouseMove (Point2d.pixels (Tuple.first clientPos) (Tuple.second clientPos))
                                                    )
                                                ]

                                            MouseLeftUp ->
                                                []
                                       )
                                )
                                []
                    ]
                    (Element.html (canvasView loadedModel))
        ]
    }


findPixelPerfectSize : FrontendLoaded -> { canvasSize : ( Int, Int ), actualCanvasSize : ( Int, Int ) }
findPixelPerfectSize frontendModel =
    let
        (Quantity pixelRatio) =
            frontendModel.devicePixelRatio

        findValue : Quantity Int Pixels -> ( Int, Int )
        findValue value =
            List.range 0 9
                |> List.map ((+) (Pixels.inPixels value))
                |> List.find
                    (\v ->
                        let
                            a =
                                toFloat v * pixelRatio
                        in
                        a == toFloat (round a) && modBy 2 (round a) == 0
                    )
                |> Maybe.map (\v -> ( v, toFloat v * pixelRatio |> round ))
                |> Maybe.withDefault ( Pixels.inPixels value, toFloat (Pixels.inPixels value) * pixelRatio |> round )

        ( w, actualW ) =
            findValue (Tuple.first frontendModel.windowSize)

        ( h, actualH ) =
            findValue (Tuple.second frontendModel.windowSize)
    in
    { canvasSize = ( w, h ), actualCanvasSize = ( actualW, actualH ) }


canvasView : FrontendLoaded -> Html FrontendMsg
canvasView model =
    let
        ( windowWidth, windowHeight ) =
            actualCanvasSize

        ( cssWindowWidth, cssWindowHeight ) =
            canvasSize

        { canvasSize, actualCanvasSize } =
            findPixelPerfectSize model

        { x, y } =
            Point2d.unwrap (actualViewPoint model)

        viewMatrix =
            Mat4.makeScale3 (2 / toFloat windowWidth) (-2 / toFloat windowHeight) 1
                |> Mat4.translate3
                    (negate <| toFloat <| round x)
                    (negate <| toFloat <| round y)
                    0
    in
    WebGL.toHtmlWith
        [ WebGL.alpha False, WebGL.antialias ]
        [ Html.Attributes.width windowWidth
        , Html.Attributes.height windowHeight
        , Html.Attributes.style "width" (String.fromInt cssWindowWidth ++ "px")
        , Html.Attributes.style "height" (String.fromInt cssWindowHeight ++ "px")
        ]
        (WebGL.entity
            Cursor.vertexShader
            Cursor.fragmentShader
            Cursor.mesh
            { view = viewMatrix
            , offset = Units.asciiToWorld model.cursor.position |> Helper.coordToVec
            , color = Math.Vector3.vec3 1 1 0
            }
            :: (Maybe.map (drawText model.grid viewMatrix) model.texture |> Maybe.withDefault [])
        )


drawText : Grid -> Mat4 -> Texture -> List WebGL.Entity
drawText grid viewMatrix texture =
    Grid.meshes grid
        |> List.map
            (\mesh ->
                WebGL.entityWith
                    [ WebGL.Settings.cullFace WebGL.Settings.back
                    , Blend.add Blend.one Blend.oneMinusSrcAlpha
                    ]
                    vertexShader
                    fragmentShader
                    mesh
                    { view = viewMatrix
                    , texture = texture
                    , color = Math.Vector3.vec3 0 0 0
                    }
            )


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    case model of
        Loading _ ->
            Sub.none

        Loaded loadedModel ->
            Sub.batch
                [ Sub.map KeyMsg Keyboard.subscriptions
                , Keyboard.downs KeyDown
                , if Keyboard.Arrows.arrowsDirection loadedModel.pressedKeys == Keyboard.Arrows.NoDirection then
                    Sub.none

                  else
                    Browser.Events.onAnimationFrame Step
                , Browser.Events.onResize (\width height -> WindowResized ( Pixels.pixels width, Pixels.pixels height ))
                , martinsstewart_elm_device_pixel_ratio_from_js (Units.worldUnit >> Quantity.per Pixels.pixel >> GotDevicePixelRatio)
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
