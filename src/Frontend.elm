port module Frontend exposing (app, init, update, updateFromBackend, view)

import Array
import Ascii
import BoundingBox2d exposing (BoundingBox2d)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import ColorIndex
import Cursor
import Dict exposing (Dict)
import Duration
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
import Html.Events.Extra.Mouse exposing (Button(..))
import Html.Events.Extra.Touch
import Keyboard
import Keyboard.Arrows
import Lamdera
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import LocalModel exposing (LocalModel)
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
import User exposing (User, UserId(..))
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


loadedInit : FrontendLoading -> LoadingData_ -> ( FrontendModel, Cmd FrontendMsg )
loadedInit loading { grid, user, otherUsers, undoHistory, redoHistory } =
    let
        cursor =
            Cursor.setCursor ( Units.asciiUnit 0, Units.asciiUnit 0 )
    in
    ( Loaded
        { key = loading.key
        , localModel = LocalModel.init { grid = grid, undoHistory = undoHistory, redoHistory = redoHistory }
        , meshes =
            Grid.allCells grid
                |> List.map
                    (\( cellCoord, cell ) ->
                        ( Helper.toRawCoord cellCoord
                        , Grid.mesh cellCoord (GridCell.flatten cell |> Array.toList)
                        )
                    )
                |> Dict.fromList
        , cursorMesh = Cursor.toMesh cursor
        , viewPoint = Point2d.origin
        , cursor = cursor
        , texture = Nothing
        , pressedKeys = []
        , windowSize = loading.windowSize
        , devicePixelRatio = loading.devicePixelRatio
        , zoomFactor = loading.zoomFactor
        , mouseLeft = MouseButtonUp
        , mouseMiddle = MouseButtonUp
        , user = user
        , otherUsers = otherUsers
        , pendingChanges = []
        , tool = DragTool
        , undoAddLast = Time.millisToPosix 0
        , time = Time.millisToPosix 0
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
            updateLoaded msg frontendLoaded
                |> Tuple.mapFirst (updateMeshes frontendLoaded >> Cursor.updateMesh frontendLoaded)
                |> Tuple.mapFirst Loaded


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

        UrlChanged _ ->
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
                        copyText model

                    else
                        ( model, Cmd.none )

                Just (Keyboard.Character "x") ->
                    if keyDown Keyboard.Control model || keyDown Keyboard.Meta model then
                        cutText model

                    else
                        ( model, Cmd.none )

                Just (Keyboard.Character "z") ->
                    if keyDown Keyboard.Control model || keyDown Keyboard.Meta model then
                        ( updateLocalModel LocalUndo model, Cmd.none )

                    else
                        ( model, Cmd.none )

                Just (Keyboard.Character "Z") ->
                    if keyDown Keyboard.Control model || keyDown Keyboard.Meta model then
                        ( updateLocalModel LocalRedo model, Cmd.none )

                    else
                        ( model, Cmd.none )

                Just (Keyboard.Character "y") ->
                    if keyDown Keyboard.Control model || keyDown Keyboard.Meta model then
                        ( updateLocalModel LocalRedo model, Cmd.none )

                    else
                        ( model, Cmd.none )

                Just Keyboard.Delete ->
                    let
                        bounds =
                            Cursor.bounds model.cursor
                    in
                    ( clearTextSelection bounds model
                    , Cmd.none
                    )

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

        MouseDown button mousePosition ->
            ( if button == MainButton then
                { model
                    | mouseLeft =
                        MouseButtonDown
                            { start = mousePosition, start_ = screenToWorld model mousePosition, current = mousePosition }
                }

              else if button == MiddleButton then
                { model
                    | mouseMiddle =
                        MouseButtonDown
                            { start = mousePosition, start_ = screenToWorld model mousePosition, current = mousePosition }
                }

              else
                model
            , Browser.Dom.focus "textareaId" |> Task.attempt (\_ -> NoOpFrontendMsg)
            )

        MouseUp button mousePosition ->
            case ( button, model.mouseLeft, model.mouseMiddle ) of
                ( MainButton, MouseButtonDown mouseState, _ ) ->
                    ( { model
                        | mouseLeft = MouseButtonUp
                        , viewPoint =
                            case ( model.mouseMiddle, model.tool ) of
                                ( MouseButtonUp, DragTool ) ->
                                    offsetViewPoint model mouseState.start mousePosition

                                _ ->
                                    model.viewPoint
                        , cursor =
                            if
                                Vector2d.from mouseState.start mousePosition
                                    |> Vector2d.length
                                    |> Quantity.lessThan (Pixels.pixels 15)
                            then
                                screenToWorld model mousePosition |> Units.worldToAscii |> Cursor.setCursor

                            else
                                model.cursor
                      }
                    , Cmd.none
                    )

                ( MiddleButton, _, MouseButtonDown mouseState ) ->
                    ( { model
                        | mouseMiddle = MouseButtonUp
                        , viewPoint = offsetViewPoint model mouseState.start mousePosition
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        MouseMove mousePosition ->
            ( { model
                | mouseLeft =
                    case model.mouseLeft of
                        MouseButtonDown mouseState ->
                            MouseButtonDown { mouseState | current = mousePosition }

                        MouseButtonUp ->
                            MouseButtonUp
                , mouseMiddle =
                    case model.mouseMiddle of
                        MouseButtonDown mouseState ->
                            MouseButtonDown { mouseState | current = mousePosition }

                        MouseButtonUp ->
                            MouseButtonUp
                , cursor =
                    case ( model.mouseLeft, model.tool ) of
                        ( MouseButtonDown mouseState, SelectTool ) ->
                            Cursor.selection
                                (mouseState.start_ |> Units.worldToAscii)
                                (screenToWorld model mousePosition |> Units.worldToAscii)

                        _ ->
                            model.cursor
              }
            , Cmd.none
            )

        ShortIntervalElapsed time ->
            let
                model_ =
                    { model | time = time }
            in
            case List.Nonempty.fromList model_.pendingChanges of
                Just nonempty ->
                    ( { model_ | pendingChanges = [] }
                    , GridChange nonempty |> Lamdera.sendToBackend
                    )

                Nothing ->
                    ( model_, Cmd.none )

        ZoomFactorPressed zoomFactor ->
            ( { model | zoomFactor = zoomFactor }, Cmd.none )

        SelectToolPressed toolType ->
            ( { model | tool = toolType }, Cmd.none )

        UndoPressed ->
            ( updateLocalModel LocalUndo model, Cmd.none )

        RedoPressed ->
            ( updateLocalModel LocalRedo model, Cmd.none )

        CopyPressed ->
            copyText model

        CutPressed ->
            cutText model


copyText : FrontendLoaded -> ( FrontendLoaded, Cmd FrontendMsg )
copyText model =
    ( model
    , LocalModel.localModel model.localModel
        |> .grid
        |> selectionToString (Cursor.bounds model.cursor)
        |> supermario_copy_to_clipboard_to_js
    )


cutText : FrontendLoaded -> ( FrontendLoaded, Cmd FrontendMsg )
cutText model =
    let
        bounds =
            Cursor.bounds model.cursor
    in
    ( clearTextSelection bounds model
    , LocalModel.localModel model.localModel
        |> .grid
        |> selectionToString bounds
        |> supermario_copy_to_clipboard_to_js
    )


updateLocalModel : LocalChange -> FrontendLoaded -> FrontendLoaded
updateLocalModel msg model =
    { model
        | pendingChanges = model.pendingChanges ++ [ msg ]
        , localModel =
            LocalModel.update
                (localModelConfig (User.id model.user))
                (LocalChange msg)
                model.localModel
    }


isGridChange : LocalChange -> Bool
isGridChange localChange =
    case localChange of
        LocalGridChange _ ->
            True

        _ ->
            False


clearTextSelection bounds model =
    let
        ( w, h ) =
            bounds.max |> Helper.minusTuple bounds.min |> Helper.toRawCoord
    in
    { model | cursor = Cursor.setCursor bounds.min }
        |> changeText (String.repeat w " " |> List.repeat h |> String.join "\n")
        |> (\m -> { m | cursor = model.cursor })


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
    String.left 5000 text
        |> String.filter ((/=) '\u{000D}')
        |> String.split "\n"
        |> List.Nonempty.fromList
        |> Maybe.map (List.Nonempty.map (String.toList >> List.map (Ascii.fromChar >> Maybe.withDefault Ascii.default)))
        |> Maybe.map
            (\lines ->
                let
                    model_ =
                        if Duration.from model.undoAddLast model.time |> Quantity.greaterThan (Duration.seconds 2) then
                            updateLocalModel AddUndo { model | undoAddLast = model.time }

                        else
                            model
                in
                Grid.textToChange (Cursor.position model_.cursor) lines
                    |> List.Nonempty.map LocalGridChange
                    |> List.Nonempty.foldl updateLocalModel
                        { model_
                            | cursor =
                                Cursor.moveCursor
                                    False
                                    ( Units.asciiUnit (List.Nonempty.last lines |> List.length)
                                    , Units.asciiUnit (List.Nonempty.length lines - 1)
                                    )
                                    model_.cursor
                        }
            )
        |> Maybe.withDefault model


keyDown : Keyboard.Key -> { a | pressedKeys : List Keyboard.Key } -> Bool
keyDown key { pressedKeys } =
    List.any ((==) key) pressedKeys


updateMeshes : FrontendLoaded -> FrontendLoaded -> FrontendLoaded
updateMeshes oldModel newModel =
    let
        oldCells =
            LocalModel.localModel oldModel.localModel |> .grid |> Grid.allCellsDict

        newCells =
            LocalModel.localModel newModel.localModel |> .grid |> Grid.allCellsDict
    in
    { newModel
        | meshes =
            Dict.map
                (\coord newCell ->
                    case Dict.get coord oldCells of
                        Just oldCell ->
                            if oldCell == newCell then
                                case Dict.get coord newModel.meshes of
                                    Just mesh ->
                                        mesh

                                    Nothing ->
                                        Grid.mesh
                                            (Helper.fromRawCoord coord)
                                            (GridCell.flatten newCell |> Array.toList)

                            else
                                Grid.mesh (Helper.fromRawCoord coord) (GridCell.flatten newCell |> Array.toList)

                        Nothing ->
                            Grid.mesh (Helper.fromRawCoord coord) (GridCell.flatten newCell |> Array.toList)
                )
                newCells
    }


offsetViewPoint :
    FrontendLoaded
    -> Point2d Pixels ScreenCoordinate
    -> Point2d Pixels ScreenCoordinate
    -> Point2d WorldPixel WorldCoordinate
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
    case ( model.mouseLeft, model.mouseMiddle ) of
        ( _, MouseButtonDown { start, current } ) ->
            offsetViewPoint model start current

        ( MouseButtonDown { start, current }, _ ) ->
            if model.tool == DragTool then
                offsetViewPoint model start current

            else
                model.viewPoint

        _ ->
            model.viewPoint


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case ( model, msg ) of
        ( Loading loading, LoadingData loadingData ) ->
            loadedInit loading loadingData

        ( Loaded loaded, _ ) ->
            updateLoadedFromBackend msg loaded |> Tuple.mapFirst (updateMeshes loaded) |> Tuple.mapFirst Loaded

        _ ->
            ( model, Cmd.none )


localModelConfig : UserId -> LocalModel.Config Change LocalGrid
localModelConfig userId =
    { msgEqual = \msg0 msg1 -> msg0 == msg1
    , update =
        \msg model ->
            case msg of
                LocalChange (LocalGridChange gridChange) ->
                    { model
                        | redoHistory = []
                        , grid = Grid.addChange (Grid.localChangeToChange userId gridChange) model.grid
                    }

                LocalChange LocalRedo ->
                    case model.redoHistory of
                        head :: rest ->
                            { model
                                | undoHistory = Grid.undoPoint userId model.grid :: model.undoHistory
                                , redoHistory = rest
                                , grid = Grid.setUndoPoints userId head model.grid
                            }

                        [] ->
                            model

                LocalChange LocalUndo ->
                    case model.undoHistory of
                        head :: rest ->
                            { model
                                | undoHistory = rest
                                , redoHistory = Grid.undoPoint userId model.grid :: model.redoHistory
                                , grid = Grid.setUndoPoints userId head model.grid
                            }

                        [] ->
                            model

                LocalChange AddUndo ->
                    { model
                        | redoHistory = []
                        , undoHistory = Grid.undoPoint userId model.grid :: model.undoHistory
                    }

                ServerChange (ServerGridChange gridChange) ->
                    { model | grid = Grid.addChange gridChange model.grid }

                ServerChange (ServerUndoPoint undoPoint) ->
                    { model | grid = Grid.setUndoPoints undoPoint.userId undoPoint.undoPoints model.grid }
    }


updateLoadedFromBackend : ToFrontend -> FrontendLoaded -> ( FrontendLoaded, Cmd FrontendMsg )
updateLoadedFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        LoadingData _ ->
            ( model, Cmd.none )

        ServerChangeBroadcast changeBroadcast ->
            let
                newLocalModel =
                    LocalModel.updateFromBackend
                        (localModelConfig (User.id model.user))
                        (List.Nonempty.map ServerChange changeBroadcast)
                        model.localModel
            in
            ( { model | localModel = newLocalModel }
            , Cmd.none
            )

        NewUserBroadcast user ->
            ( if User.id user == User.id model.user then
                model

              else
                { model | otherUsers = model.otherUsers ++ [ user ] }
            , Cmd.none
            )

        UserModifiedBroadcast user ->
            ( if User.id user == User.id model.user then
                { model | user = user }

              else
                { model | otherUsers = List.updateIf (User.id >> (==) (User.id user)) (always user) model.otherUsers }
            , Cmd.none
            )

        LocalChangeResponse changes ->
            ( { model
                | localModel =
                    LocalModel.updateFromBackend
                        (localModelConfig (User.id model.user))
                        (List.Nonempty.map LocalChange changes)
                        model.localModel
              }
            , Cmd.none
            )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    let
        touchEvent msg =
            \event ->
                case event.touches of
                    head :: _ ->
                        let
                            ( x, y ) =
                                head.pagePos
                        in
                        msg (Point2d.pixels x y)

                    [] ->
                        NoOpFrontendMsg
    in
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
                                 , Html.Attributes.style "opacity" "0"
                                 , Html.Attributes.id "textareaId"

                                 --, Html.Events.Extra.Touch.onWithOptions "touchstart" { stopPropagation = False, preventDefault = False } (touchEvent (MouseDown MainButton))
                                 --, Html.Events.Extra.Touch.onWithOptions "touchmove" { stopPropagation = False, preventDefault = False } (touchEvent MouseMove)
                                 , Html.Events.Extra.Touch.onWithOptions "touchend" { stopPropagation = False, preventDefault = True } (touchEvent (MouseUp MainButton >> Debug.log ""))
                                 , Html.Events.Extra.Touch.onWithOptions "touchcancel" { stopPropagation = False, preventDefault = True } (touchEvent (MouseUp MainButton >> Debug.log ""))
                                 , Html.Events.Extra.Mouse.onDown
                                    (\{ clientPos, button } ->
                                        MouseDown button (Point2d.pixels (Tuple.first clientPos) (Tuple.second clientPos))
                                    )
                                 , Html.Events.Extra.Mouse.onUp
                                    (\{ clientPos, button } ->
                                        MouseUp button (Point2d.pixels (Tuple.first clientPos) (Tuple.second clientPos))
                                    )
                                 ]
                                    ++ (case ( loadedModel.mouseLeft, loadedModel.mouseMiddle ) of
                                            ( MouseButtonDown _, _ ) ->
                                                [ mouseMoveAttribute
                                                ]

                                            ( _, MouseButtonDown _ ) ->
                                                [ mouseMoveAttribute
                                                ]

                                            _ ->
                                                []
                                       )
                                )
                                []
                    , Element.inFront (toolbarView loadedModel)

                    --, Element.inFront (userListView loadedModel)
                    ]
                    (Element.html (canvasView loadedModel))
        ]
    }


mouseMoveAttribute : Html.Attribute FrontendMsg
mouseMoveAttribute =
    Html.Events.Extra.Mouse.onMove
        (\{ clientPos } ->
            MouseMove (Point2d.pixels (Tuple.first clientPos) (Tuple.second clientPos))
        )


userListView : FrontendLoaded -> Element FrontendMsg
userListView model =
    Element.column
        [ Element.Background.color lightColor, Element.alignRight, Element.spacing 8, Element.padding 8 ]
        (Element.row
            [ Element.spacing 8 ]
            [ Element.el
                [ Element.width (Element.px 16)
                , Element.height (Element.px 16)
                , Element.Background.color <| ColorIndex.toColor <| User.color model.user
                ]
                Element.none
            , Element.row
                [ Element.spacing 8 ]
                [ Element.text (User.name model.user), Element.el [ Element.Font.bold ] (Element.text "(you)") ]
            ]
            :: List.map
                (\otherUser ->
                    Element.row [ Element.spacing 8 ]
                        [ Element.el
                            [ Element.width (Element.px 16)
                            , Element.height (Element.px 16)
                            , Element.Background.color <| ColorIndex.toColor <| User.color otherUser
                            ]
                            Element.none
                        , Element.row
                            [ Element.spacing 8 ]
                            [ Element.text (User.name otherUser) ]
                        ]
                )
                model.otherUsers
        )


toolbarView : FrontendLoaded -> Element FrontendMsg
toolbarView model =
    let
        zoomView =
            List.range 1 3
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

        toolView =
            List.map
                (\( tool, icon ) ->
                    toolbarButton
                        [ if model.tool == tool then
                            Element.Background.color buttonHighlight

                          else
                            Element.Background.color buttonDefault
                        ]
                        (SelectToolPressed tool)
                        icon
                )
                tools

        undoRedoView =
            [ toolbarButton
                []
                UndoPressed
                (Element.image
                    [ Element.width (Element.px 20)
                    , if LocalModel.localModel model.localModel |> .undoHistory |> List.isEmpty then
                        Element.alpha 0.5

                      else
                        Element.alpha 1
                    ]
                    { src = "undo.svg", description = "Undo button" }
                )
            , toolbarButton
                []
                RedoPressed
                (Element.image
                    [ Element.width (Element.px 20)
                    , if LocalModel.localModel model.localModel |> .redoHistory |> List.isEmpty then
                        Element.alpha 0.5

                      else
                        Element.alpha 1
                    ]
                    { src = "redo.svg", description = "Undo button" }
                )
            ]

        copyView =
            [ toolbarButton
                []
                CopyPressed
                (Element.image
                    [ Element.width (Element.px 20) ]
                    { src = "copy.svg", description = "Copy text button" }
                )
            , toolbarButton
                []
                CutPressed
                (Element.image
                    [ Element.width (Element.px 20) ]
                    { src = "cut.svg", description = "Cut text button" }
                )
            ]

        groups =
            [ Element.el [ Element.paddingXY 2 0 ] (Element.text "🔎") :: zoomView
            , toolView
            , undoRedoView
            , copyView
            ]
                |> List.map (Element.row [ Element.spacing 2 ])
    in
    Element.wrappedRow
        [ Element.Background.color lightColor
        , Element.spacing 8
        , Element.padding 8
        , Element.Border.color backgroundColor
        , Element.Border.width 2
        , Element.Font.color textColor
        ]
        groups
        |> Element.el
            [ Element.paddingXY 8 0
            , Element.alignBottom
            , Element.centerX
            , Element.moveUp 8
            ]


tools : List ( ToolType, Element msg )
tools =
    [ ( DragTool
      , Element.image
            [ Element.width (Element.px 20) ]
            { src = "4-direction-arrows.svg", description = "Drag tool" }
      )
    , ( SelectTool
      , Element.el
            [ Element.Border.width 2
            , Element.Border.dashed
            , Element.width (Element.px 20)
            , Element.height (Element.px 20)
            ]
            Element.none
      )

    --, ( RectangleTool
    --  , Element.el
    --        [ Element.Border.width 2, Element.width (Element.px 20), Element.height (Element.px 20) ]
    --        Element.none
    --  )
    ]


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
        (Cursor.draw viewMatrix (User.color model.user |> ColorIndex.toColor) model
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
