port module Frontend exposing (app, init, update, updateFromBackend, view)

import Array
import Ascii
import BoundingBox2d exposing (BoundingBox2d)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Cursor
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Grid exposing (Grid)
import GridCell
import Helper exposing (Coord)
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
import Quantity exposing (Quantity(..), Rate)
import Task
import Time
import Types exposing (..)
import Units exposing (AsciiUnit, ScreenCoordinate, WorldCoordinate, WorldPixel)
import Url
import User exposing (UserId(..))
import Vector2d exposing (Vector2d)
import WebGL exposing (Shader)
import WebGL.Settings
import WebGL.Settings.Blend as Blend
import WebGL.Texture exposing (Texture)


port martinsstewart_elm_device_pixel_ratio_from_js : (Float -> msg) -> Sub msg


port martinsstewart_elm_device_pixel_ratio_to_js : () -> Cmd msg


port supermario_copy_to_clipboard_to_js : String -> Cmd msg


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


loadedInit : FrontendLoading -> Grid -> UserId -> ( FrontendModel, Cmd FrontendMsg )
loadedInit loading grid userId =
    ( Loaded
        { key = loading.key
        , grid = grid
        , meshes =
            Grid.allCells grid
                |> List.map
                    (\( cellCoord, cell ) ->
                        ( Helper.toRawCoord cellCoord
                        , Grid.mesh cellCoord (GridCell.flatten cell |> Array.toList)
                        )
                    )
                |> Dict.fromList
        , viewPoint = Point2d.origin
        , cursor = Cursor.setCursor ( Units.asciiUnit 0, Units.asciiUnit 0 )
        , texture = Nothing
        , pressedKeys = []
        , windowSize = loading.windowSize
        , devicePixelRatio = loading.devicePixelRatio
        , zoomFactor = loading.zoomFactor
        , mouseState = MouseLeftUp
        , userId = userId
        , pendingChanges = []
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
        , Browser.Dom.focus "textareaId" |> Task.attempt (\_ -> NoOpFrontendMsg)
        ]
    )


init : Url.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ key =
    ( Loading
        { key = key
        , windowSize = ( Pixels.pixels 1920, Pixels.pixels 1080 )
        , devicePixelRatio = Quantity 1
        , zoomFactor = 1
        }
    , Cmd.batch
        [ Lamdera.sendToBackend RequestData
        , Task.perform
            (\{ viewport } ->
                WindowResized
                    ( round viewport.width |> Pixels.pixels
                    , round viewport.height |> Pixels.pixels
                    )
            )
            Browser.Dom.getViewport
        ]
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case model of
        Loading loadingModel ->
            case msg of
                WindowResized windowSize ->
                    windowResizedUpdate windowSize loadingModel |> Tuple.mapFirst Loading

                GotDevicePixelRatio devicePixelRatio ->
                    devicePixelRatioUpdate devicePixelRatio loadingModel |> Tuple.mapFirst Loading

                _ ->
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
            case Keyboard.anyKeyOriginal rawKey of
                Just (Keyboard.Character "c") ->
                    if keyDown Keyboard.Control model || keyDown Keyboard.Meta model then
                        ( model, selectionToString (Cursor.bounds model.cursor) model.grid |> supermario_copy_to_clipboard_to_js )

                    else
                        ( model, Cmd.none )

                Just (Keyboard.Character "x") ->
                    if keyDown Keyboard.Control model || keyDown Keyboard.Meta model then
                        let
                            bounds =
                                Cursor.bounds model.cursor

                            ( w, h ) =
                                bounds.max |> Helper.minusTuple bounds.min |> Helper.toRawCoord
                        in
                        ( { model | cursor = Cursor.setCursor bounds.min }
                            |> changeText (String.repeat w " " |> List.repeat h |> String.join "\n")
                            |> (\m -> { m | cursor = model.cursor })
                        , selectionToString bounds model.grid |> supermario_copy_to_clipboard_to_js
                        )

                    else
                        ( model, Cmd.none )

                Just Keyboard.ArrowLeft ->
                    ( { model
                        | cursor =
                            Cursor.moveCursor
                                (keyDown Keyboard.Shift model)
                                ( Units.asciiUnit -1, Units.asciiUnit 0 )
                                model.cursor
                      }
                    , Cmd.none
                    )

                Just Keyboard.ArrowRight ->
                    ( { model
                        | cursor =
                            Cursor.moveCursor
                                (keyDown Keyboard.Shift model)
                                ( Units.asciiUnit 1, Units.asciiUnit 0 )
                                model.cursor
                      }
                    , Cmd.none
                    )

                Just Keyboard.ArrowUp ->
                    ( { model
                        | cursor =
                            Cursor.moveCursor
                                (keyDown Keyboard.Shift model)
                                ( Units.asciiUnit 0, Units.asciiUnit -1 )
                                model.cursor
                      }
                    , Cmd.none
                    )

                Just Keyboard.ArrowDown ->
                    ( { model
                        | cursor =
                            Cursor.moveCursor
                                (keyDown Keyboard.Shift model)
                                ( Units.asciiUnit 0, Units.asciiUnit 1 )
                                model.cursor
                      }
                    , Cmd.none
                    )

                Just Keyboard.Backspace ->
                    let
                        newCursor =
                            Cursor.moveCursor
                                False
                                ( Units.asciiUnit -1, Units.asciiUnit 0 )
                                model.cursor
                    in
                    ( { model | cursor = newCursor } |> changeText " " |> (\m -> { m | cursor = newCursor })
                    , Cmd.none
                    )

                Just Keyboard.Delete ->
                    ( changeText " " model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Step _ ->
            ( model, Cmd.none )

        WindowResized windowSize ->
            windowResizedUpdate windowSize model

        GotDevicePixelRatio devicePixelRatio ->
            devicePixelRatioUpdate devicePixelRatio model

        UserTyped text ->
            if text == "\n" || text == "\n\u{000D}" then
                ( { model | cursor = Cursor.newLine model.cursor }, Cmd.none )

            else
                ( changeText text model, Cmd.none )

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
                            if
                                Vector2d.from start mousePosition
                                    |> Vector2d.length
                                    |> Quantity.lessThan (Pixels.pixels 15)
                            then
                                screenToWorld model mousePosition |> Units.worldToAscii |> Cursor.setCursor

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

        ShortIntervalElapsed _ ->
            ( { model | pendingChanges = [] }
            , case List.Nonempty.fromList model.pendingChanges of
                Just nonempty ->
                    Lamdera.sendToBackend (GridChange { changes = nonempty })

                Nothing ->
                    Cmd.none
            )

        ZoomFactorPressed zoomFactor ->
            ( { model | zoomFactor = zoomFactor }, Cmd.none )


screenToWorld : FrontendLoaded -> Point2d Pixels ScreenCoordinate -> Point2d WorldPixel WorldCoordinate
screenToWorld model =
    let
        ( w, h ) =
            model.windowSize
    in
    Point2d.translateBy
        (Vector2d.xy (Quantity.toFloatQuantity w) (Quantity.toFloatQuantity h) |> Vector2d.scaleBy -0.5)
        >> Point2d.at (Quantity.divideBy (toFloat model.zoomFactor) model.devicePixelRatio)
        >> Point2d.placeIn (Units.screenFrame (actualViewPoint model))


selectionToString : { min : Coord AsciiUnit, max : Coord AsciiUnit } -> Grid -> String
selectionToString bounds grid =
    let
        minCell =
            Grid.asciiToCellAndLocalCoord bounds.min |> Tuple.first

        maxCell =
            Grid.asciiToCellAndLocalCoord bounds.max |> Tuple.first

        flattenedCells =
            Helper.coordRangeFold
                (\coord dict ->
                    case Grid.getCell coord grid of
                        Just cell ->
                            Dict.insert (Helper.toRawCoord coord) (GridCell.flatten cell) dict

                        Nothing ->
                            dict
                )
                identity
                minCell
                maxCell
                Dict.empty
    in
    Helper.coordRangeFoldReverse
        (\coord chars ->
            let
                ( cellCoord, localCoord ) =
                    Grid.asciiToCellAndLocalCoord coord
            in
            (Dict.get (Helper.toRawCoord cellCoord) flattenedCells
                |> Maybe.andThen (Array.get localCoord)
                |> Maybe.withDefault Ascii.default
                |> Ascii.toChar
            )
                :: chars
        )
        ((::) '\n')
        bounds.min
        (bounds.max |> Helper.minusTuple ( Units.asciiUnit 1, Units.asciiUnit 1 ))
        []
        |> String.fromList


windowResizedUpdate : Coord Pixels -> { b | windowSize : Coord Pixels } -> ( { b | windowSize : Coord Pixels }, Cmd msg )
windowResizedUpdate windowSize model =
    ( { model | windowSize = windowSize }, martinsstewart_elm_device_pixel_ratio_to_js () )


devicePixelRatioUpdate :
    Quantity Float (Rate WorldPixel Pixels)
    -> { b | devicePixelRatio : Quantity Float (Rate WorldPixel Pixels), zoomFactor : Int }
    -> ( { b | devicePixelRatio : Quantity Float (Rate WorldPixel Pixels), zoomFactor : Int }, Cmd msg )
devicePixelRatioUpdate devicePixelRatio model =
    ( { model
        | devicePixelRatio = devicePixelRatio
        , zoomFactor = toFloat model.zoomFactor * Quantity.ratio devicePixelRatio model.devicePixelRatio |> round
      }
    , Cmd.none
    )


changeText : String -> FrontendLoaded -> FrontendLoaded
changeText text model =
    String.filter ((/=) '\u{000D}') text
        |> String.split "\n"
        |> List.Nonempty.fromList
        |> Maybe.map (List.Nonempty.map (String.toList >> List.map (Ascii.fromChar >> Maybe.withDefault Ascii.default)))
        |> Maybe.map
            (\lines ->
                let
                    changes =
                        Grid.textToChange (Cursor.position model.cursor) lines

                    newGrid =
                        Grid.addChange model.userId changes model.grid
                in
                { model
                    | grid = newGrid
                    , meshes = updateMeshes changes newGrid model.meshes
                    , cursor =
                        Cursor.moveCursor
                            (keyDown Keyboard.Shift model)
                            ( Units.asciiUnit (List.Nonempty.last lines |> List.length)
                            , Units.asciiUnit (List.Nonempty.length lines - 1)
                            )
                            model.cursor
                    , pendingChanges = model.pendingChanges ++ changes
                }
            )
        |> Maybe.withDefault model


keyDown : Keyboard.Key -> { a | pressedKeys : List Keyboard.Key } -> Bool
keyDown key { pressedKeys } =
    List.any ((==) key) pressedKeys


updateMeshes :
    List { a | cellPosition : Helper.Coord Units.CellUnit }
    -> Grid
    -> Dict ( Int, Int ) (WebGL.Mesh { position : Vec2, texturePosition : Vec2 })
    -> Dict ( Int, Int ) (WebGL.Mesh { position : Vec2, texturePosition : Vec2 })
updateMeshes changes grid meshes =
    changes
        |> List.map (\{ cellPosition } -> ( Helper.toRawCoord cellPosition, cellPosition ))
        |> Dict.fromList
        |> Dict.values
        |> List.foldl
            (\cellCoord meshes_ ->
                case Grid.getCell cellCoord grid of
                    Just cell ->
                        GridCell.flatten cell
                            |> Array.toList
                            |> Grid.mesh cellCoord
                            |> (\mesh -> Dict.insert (Helper.toRawCoord cellCoord) mesh meshes_)

                    Nothing ->
                        meshes_
            )
            meshes


offsetViewPoint : FrontendLoaded -> Point2d Pixels ScreenCoordinate -> Point2d Pixels ScreenCoordinate -> Point2d WorldPixel WorldCoordinate
offsetViewPoint { windowSize, viewPoint, devicePixelRatio, zoomFactor } mouseStart mouseCurrent =
    let
        delta : Vector2d WorldPixel WorldCoordinate
        delta =
            Vector2d.from mouseCurrent mouseStart
                |> Vector2d.at (Quantity.divideBy (toFloat zoomFactor) devicePixelRatio)
                |> Vector2d.placeIn (Units.screenFrame viewPoint)
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

        ( Loading loading, LoadingData { grid, userId } ) ->
            loadedInit loading grid userId

        ( Loaded loaded, GridChangeBroadcast { changes, user } ) ->
            let
                newGrid =
                    List.Nonempty.foldl
                        (\change grid ->
                            Grid.addChangeBroadcast user change grid |> Maybe.withDefault grid
                        )
                        loaded.grid
                        (List.Nonempty.reverse changes)
            in
            ( Loaded
                { loaded
                    | grid = newGrid
                    , meshes = updateMeshes (List.Nonempty.toList changes) newGrid loaded.meshes
                }
            , Cmd.none
            )

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
                    , Element.inFront (toolbarView loadedModel)
                    ]
                    (Element.html (canvasView loadedModel))
        ]
    }


toolbarView : FrontendLoaded -> Element FrontendMsg
toolbarView model =
    Element.row
        [ Element.Background.color lightColor
        , Element.alignBottom
        , Element.centerX
        , Element.moveUp 16
        , Element.spacing 8
        , Element.padding 8
        , Element.Border.color backgroundColor
        , Element.Border.width 2
        , Element.Font.color textColor
        ]
        (Element.text "🔎"
            :: (List.range 1 3
                    |> List.map
                        (\zoom ->
                            toolbarButton
                                [ if model.zoomFactor == zoom then
                                    Element.Background.color buttonHighlight

                                  else
                                    Element.Background.color buttonDefault
                                ]
                                (ZoomFactorPressed zoom)
                                (Element.text (String.fromInt zoom ++ "x"))
                        )
               )
            ++ [ verticalSeparator
               ]
        )


lightColor : Element.Color
lightColor =
    Element.rgb255 239 249 240


backgroundColor : Element.Color
backgroundColor =
    Element.rgb255 107 77 87


buttonHighlight : Element.Color
buttonHighlight =
    Element.rgb255 221 200 196


buttonDefault : Element.Color
buttonDefault =
    Element.rgb255 167 129 123


textColor : Element.Color
textColor =
    Element.rgb255 19 7 12


verticalSeparator : Element msg
verticalSeparator =
    Element.el
        [ Element.width (Element.px 2)
        , Element.height Element.fill
        , Element.Background.color backgroundColor
        ]
        Element.none


toolbarButton : List (Element.Attribute msg) -> msg -> Element msg -> Element msg
toolbarButton attrubtes onPress label =
    Element.Input.button
        ([ Element.padding 8
         , Element.Background.color buttonDefault
         , Element.mouseOver [ Element.Background.color buttonHighlight ]
         , Element.Border.width 2
         , Element.Border.color backgroundColor
         ]
            ++ attrubtes
        )
        { onPress = Just onPress, label = label }


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
        viewMin =
            screenToWorld model Point2d.origin
                |> Point2d.translateBy
                    (Helper.fromRawCoord ( -1, -1 )
                        |> Units.cellToAscii
                        |> Units.asciiToWorld
                        |> Helper.coordToVector2d
                    )

        viewMax =
            screenToWorld model (Helper.coordToPoint model.windowSize)

        viewBounds : BoundingBox2d WorldPixel WorldCoordinate
        viewBounds =
            BoundingBox2d.from viewMin viewMax

        ( windowWidth, windowHeight ) =
            actualCanvasSize

        ( cssWindowWidth, cssWindowHeight ) =
            canvasSize

        { canvasSize, actualCanvasSize } =
            findPixelPerfectSize model

        { x, y } =
            Point2d.unwrap (actualViewPoint model)

        viewMatrix =
            Mat4.makeScale3 (toFloat model.zoomFactor * 2 / toFloat windowWidth) (toFloat model.zoomFactor * -2 / toFloat windowHeight) 1
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
        , Html.Attributes.style "image-rendering" "crisp-edges"
        , Html.Attributes.style "image-rendering" "pixelated"
        ]
        (Cursor.draw viewMatrix model.cursor
            :: (Maybe.map
                    (drawText
                        (Dict.filter
                            (\key _ ->
                                Helper.fromRawCoord key
                                    |> Units.cellToAscii
                                    |> Units.asciiToWorld
                                    |> Helper.coordToPoint
                                    |> (\p -> BoundingBox2d.contains p viewBounds)
                            )
                            model.meshes
                        )
                        viewMatrix
                    )
                    model.texture
                    |> Maybe.withDefault []
               )
        )


drawText : Dict ( Int, Int ) (WebGL.Mesh Vertex) -> Mat4 -> Texture -> List WebGL.Entity
drawText meshes viewMatrix texture =
    Dict.toList meshes
        |> List.map
            (\( _, mesh ) ->
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
    Sub.batch
        [ martinsstewart_elm_device_pixel_ratio_from_js
            (Units.worldUnit >> Quantity.per Pixels.pixel >> GotDevicePixelRatio)
        , Browser.Events.onResize (\width height -> WindowResized ( Pixels.pixels width, Pixels.pixels height ))
        , case model of
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
                    , Time.every 1000 ShortIntervalElapsed
                    ]
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
