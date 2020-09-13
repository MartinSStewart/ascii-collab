port module Frontend exposing (app, init, update, updateFromBackend, view)

import Array
import Ascii exposing (Ascii)
import BoundingBox2d exposing (BoundingBox2d)
import Bounds
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Change exposing (Change(..))
import ColorIndex
import Cursor exposing (Cursor)
import Dict exposing (Dict)
import Duration
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Env
import EverySet exposing (EverySet)
import Grid exposing (Grid)
import GridCell
import Helper exposing (Coord)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse exposing (Button(..))
import Html.Events.Extra.Touch
import Keyboard
import Lamdera
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import LocalGrid
import LocalModel
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..), Rate)
import Task
import Time
import Types exposing (..)
import UiColors
import Units exposing (AsciiUnit, CellUnit, ScreenCoordinate, WorldCoordinate, WorldPixel)
import Url exposing (Url)
import Url.Parser exposing ((<?>))
import UrlHelper
import User exposing (UserData, UserId(..))
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
loadedInit loading loadingData =
    let
        cursor : Cursor
        cursor =
            Cursor.setCursor loading.viewPoint

        model =
            { key = loading.key
            , localModel = LocalGrid.init loadingData
            , meshes = Dict.empty
            , cursorMesh = Cursor.toMesh cursor
            , viewPoint = Units.asciiToWorld loading.viewPoint |> Helper.coordToPoint
            , viewPointLastInterval = Point2d.origin
            , cursor = cursor
            , texture = Nothing
            , pressedKeys = []
            , windowSize = loading.windowSize
            , devicePixelRatio = loading.devicePixelRatio
            , zoomFactor = loading.zoomFactor
            , mouseLeft = MouseButtonUp
            , mouseMiddle = MouseButtonUp
            , pendingChanges = []
            , tool = DragTool
            , undoAddLast = Time.millisToPosix 0
            , time = loading.time
            , lastTouchMove = Nothing
            , userPressHighlighted = Nothing
            , userHoverHighlighted = Nothing
            , adminEnabled = False
            }
    in
    ( updateMeshes model model
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
        |> viewBoundsUpdate
        |> Tuple.mapFirst Loaded


init : Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    let
        ( viewPoint, cmd ) =
            case Url.Parser.parse UrlHelper.urlParser url of
                Just viewPoint_ ->
                    ( viewPoint_, Cmd.none )

                Nothing ->
                    let
                        defaultViewPoint =
                            ( Units.asciiUnit 0, Units.asciiUnit 0 )
                    in
                    ( defaultViewPoint, Browser.Navigation.replaceUrl key (UrlHelper.encodeUrl defaultViewPoint) )

        -- We only load in a portion of the grid since we don't know the window size. The rest will get loaded in later anyway.
        bounds =
            Bounds.bounds
                (Grid.asciiToCellAndLocalCoord viewPoint
                    |> Tuple.first
                    |> Helper.addTuple ( Units.cellUnit -2, Units.cellUnit -2 )
                )
                (Grid.asciiToCellAndLocalCoord viewPoint
                    |> Tuple.first
                    |> Helper.addTuple ( Units.cellUnit 2, Units.cellUnit 2 )
                )
    in
    ( Loading
        { key = key
        , windowSize = ( Pixels.pixels 1920, Pixels.pixels 1080 )
        , devicePixelRatio = Quantity 1
        , zoomFactor = 1
        , time = Time.millisToPosix 0
        , viewPoint = viewPoint
        }
    , Cmd.batch
        [ Lamdera.sendToBackend (RequestData bounds)
        , Task.perform
            (\{ viewport } ->
                WindowResized
                    ( round viewport.width |> Pixels.pixels
                    , round viewport.height |> Pixels.pixels
                    )
            )
            Browser.Dom.getViewport
        , Task.perform ShortIntervalElapsed Time.now
        , cmd
        ]
    )


isTouchDevice : FrontendLoaded -> Bool
isTouchDevice model =
    model.lastTouchMove /= Nothing


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case model of
        Loading loadingModel ->
            case msg of
                WindowResized windowSize ->
                    windowResizedUpdate windowSize loadingModel |> Tuple.mapFirst Loading

                ShortIntervalElapsed time ->
                    ( Loading { loadingModel | time = time }, Cmd.none )

                GotDevicePixelRatio devicePixelRatio ->
                    devicePixelRatioUpdate devicePixelRatio loadingModel |> Tuple.mapFirst Loading

                _ ->
                    ( model, Cmd.none )

        Loaded frontendLoaded ->
            updateLoaded msg frontendLoaded
                |> Tuple.mapFirst (updateMeshes frontendLoaded >> Cursor.updateMesh frontendLoaded)
                |> viewBoundsUpdate
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
            keyMsgCanvasUpdate rawKey model

        WindowResized windowSize ->
            windowResizedUpdate windowSize model

        GotDevicePixelRatio devicePixelRatio ->
            devicePixelRatioUpdate devicePixelRatio model

        UserTyped text ->
            if cursorEnabled model then
                if text == "\n" || text == "\n\u{000D}" then
                    ( resetTouchMove model |> (\m -> { m | cursor = Cursor.newLine m.cursor }), Cmd.none )

                else
                    ( resetTouchMove model |> changeText text, Cmd.none )

            else
                ( model, Cmd.none )

        MouseDown button mousePosition ->
            let
                model_ =
                    resetTouchMove model
            in
            ( if button == MainButton then
                { model_
                    | mouseLeft =
                        MouseButtonDown
                            { start = mousePosition, start_ = screenToWorld model_ mousePosition, current = mousePosition }
                }

              else if button == MiddleButton then
                { model_
                    | mouseMiddle =
                        MouseButtonDown
                            { start = mousePosition, start_ = screenToWorld model_ mousePosition, current = mousePosition }
                }

              else
                model_
            , Browser.Dom.focus "textareaId" |> Task.attempt (\_ -> NoOpFrontendMsg)
            )

        MouseUp button mousePosition ->
            case ( button, model.mouseLeft, model.mouseMiddle ) of
                ( MainButton, MouseButtonDown mouseState, _ ) ->
                    ( mouseUp mousePosition mouseState model, Cmd.none )

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
            if isTouchDevice model then
                ( model, Cmd.none )

            else
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
                    , tool =
                        case model.tool of
                            HideUserTool _ ->
                                let
                                    localModel =
                                        LocalGrid.localModel model.localModel

                                    position : Coord AsciiUnit
                                    position =
                                        screenToWorld model mousePosition |> Units.worldToAscii

                                    ( hideUserId, _ ) =
                                        selectionPoint
                                            position
                                            localModel.hiddenUsers
                                            localModel.adminHiddenUsers
                                            localModel.grid
                                in
                                hideUserId |> Maybe.map (\a -> ( a, position )) |> HideUserTool

                            _ ->
                                model.tool
                  }
                , Cmd.none
                )

        ShortIntervalElapsed time ->
            let
                actualViewPoint_ =
                    actualViewPoint model

                model_ =
                    { model | time = time, viewPointLastInterval = actualViewPoint_ }

                urlChange =
                    if Units.worldToAscii actualViewPoint_ /= Units.worldToAscii model.viewPointLastInterval then
                        Units.worldToAscii actualViewPoint_
                            |> UrlHelper.encodeUrl
                            |> Browser.Navigation.replaceUrl model.key

                    else
                        Cmd.none
            in
            case List.Nonempty.fromList model_.pendingChanges of
                Just nonempty ->
                    ( { model_ | pendingChanges = [] }
                    , Cmd.batch
                        [ GridChange nonempty |> Lamdera.sendToBackend
                        , urlChange
                        ]
                    )

                Nothing ->
                    ( model_, urlChange )

        ZoomFactorPressed zoomFactor ->
            ( resetTouchMove model |> (\m -> { m | zoomFactor = zoomFactor }), Cmd.none )

        SelectToolPressed toolType ->
            ( resetTouchMove model |> (\m -> { m | tool = toolType }), Cmd.none )

        UndoPressed ->
            ( resetTouchMove model |> updateLocalModel Change.LocalUndo, Cmd.none )

        RedoPressed ->
            ( resetTouchMove model |> updateLocalModel Change.LocalRedo, Cmd.none )

        CopyPressed ->
            copyText model

        CutPressed ->
            cutText model

        TouchMove touchPosition ->
            let
                mouseDown m =
                    { m
                        | mouseLeft =
                            MouseButtonDown
                                { start = touchPosition
                                , start_ = screenToWorld model touchPosition
                                , current = touchPosition
                                }
                        , lastTouchMove = Just model.time
                    }
            in
            ( case model.mouseLeft of
                MouseButtonDown mouseState ->
                    let
                        duration =
                            case model.lastTouchMove of
                                Just lastTouchMove ->
                                    Duration.from lastTouchMove model.time

                                Nothing ->
                                    Quantity.zero

                        rate : Quantity Float (Rate Pixels Duration.Seconds)
                        rate =
                            Quantity.per Duration.second (Pixels.pixels 30)

                        snapDistance =
                            Pixels.pixels 50 |> Quantity.minus (Quantity.for duration rate) |> Quantity.max (Pixels.pixels 10)
                    in
                    if Point2d.distanceFrom mouseState.current touchPosition |> Quantity.greaterThan snapDistance then
                        mouseUp mouseState.current mouseState model
                            |> mouseDown

                    else
                        { model
                            | mouseLeft =
                                MouseButtonDown { mouseState | current = touchPosition }
                            , cursor =
                                case model.tool of
                                    SelectTool ->
                                        Cursor.selection
                                            (mouseState.start_ |> Units.worldToAscii)
                                            (screenToWorld model touchPosition |> Units.worldToAscii)

                                    _ ->
                                        model.cursor
                            , lastTouchMove = Just model.time
                        }

                MouseButtonUp ->
                    mouseDown model
            , Cmd.none
            )

        VeryShortIntervalElapsed time ->
            ( { model | time = time }, Cmd.none )

        UnhideUserPressed userToUnhide ->
            ( updateLocalModel
                (Change.LocalUnhideUser userToUnhide)
                { model
                    | userPressHighlighted =
                        if Just userToUnhide == model.userPressHighlighted then
                            Nothing

                        else
                            model.userPressHighlighted
                    , userHoverHighlighted =
                        if Just userToUnhide == model.userHoverHighlighted then
                            Nothing

                        else
                            model.userHoverHighlighted
                }
            , Cmd.none
            )

        UserColorSquarePressed userId ->
            ( { model
                | userPressHighlighted =
                    case model.userPressHighlighted of
                        Just userId_ ->
                            if userId_ == userId then
                                Nothing

                            else
                                Just userId

                        Nothing ->
                            Just userId
                , userHoverHighlighted =
                    if Just userId == model.userHoverHighlighted then
                        Nothing

                    else
                        model.userHoverHighlighted
              }
            , Cmd.none
            )

        UserTagMouseEntered userId ->
            ( { model | userHoverHighlighted = Just userId }, Cmd.none )

        UserTagMouseExited _ ->
            ( { model | userHoverHighlighted = Nothing }, Cmd.none )

        HideForAllTogglePressed userToHide ->
            ( updateLocalModel (Change.LocalToggleUserVisibilityForAll userToHide) model, Cmd.none )

        ToggleAdminEnabledPressed ->
            ( if Just (currentUserId model) == Env.adminUserId then
                { model | adminEnabled = not model.adminEnabled }

              else
                model
            , Cmd.none
            )


cursorEnabled : FrontendLoaded -> Bool
cursorEnabled model =
    case model.tool of
        HideUserTool _ ->
            False

        _ ->
            True


keyMsgCanvasUpdate : Keyboard.RawKey -> FrontendLoaded -> ( FrontendLoaded, Cmd FrontendMsg )
keyMsgCanvasUpdate rawKey model =
    case Keyboard.anyKeyOriginal rawKey of
        Just (Keyboard.Character "c") ->
            if keyDown Keyboard.Control model || keyDown Keyboard.Meta model then
                copyText model

            else
                ( model, Cmd.none )

        Just (Keyboard.Character "x") ->
            if keyDown Keyboard.Control model || keyDown Keyboard.Meta model then
                if cursorEnabled model then
                    cutText model

                else
                    ( model, Cmd.none )

            else
                ( model, Cmd.none )

        Just (Keyboard.Character "z") ->
            if keyDown Keyboard.Control model || keyDown Keyboard.Meta model then
                ( updateLocalModel Change.LocalUndo model, Cmd.none )

            else
                ( model, Cmd.none )

        Just (Keyboard.Character "Z") ->
            if keyDown Keyboard.Control model || keyDown Keyboard.Meta model then
                ( updateLocalModel Change.LocalRedo model, Cmd.none )

            else
                ( model, Cmd.none )

        Just (Keyboard.Character "y") ->
            if keyDown Keyboard.Control model || keyDown Keyboard.Meta model then
                ( updateLocalModel Change.LocalRedo model, Cmd.none )

            else
                ( model, Cmd.none )

        Just Keyboard.Delete ->
            if cursorEnabled model then
                let
                    bounds =
                        Cursor.bounds model.cursor
                in
                ( clearTextSelection bounds model
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Just Keyboard.ArrowLeft ->
            if cursorEnabled model then
                ( { model
                    | cursor =
                        Cursor.moveCursor
                            (keyDown Keyboard.Shift model)
                            ( Units.asciiUnit -1, Units.asciiUnit 0 )
                            model.cursor
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Just Keyboard.ArrowRight ->
            if cursorEnabled model then
                ( { model
                    | cursor =
                        Cursor.moveCursor
                            (keyDown Keyboard.Shift model)
                            ( Units.asciiUnit 1, Units.asciiUnit 0 )
                            model.cursor
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Just Keyboard.ArrowUp ->
            if cursorEnabled model then
                ( { model
                    | cursor =
                        Cursor.moveCursor
                            (keyDown Keyboard.Shift model)
                            ( Units.asciiUnit 0, Units.asciiUnit -1 )
                            model.cursor
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Just Keyboard.ArrowDown ->
            if cursorEnabled model then
                ( { model
                    | cursor =
                        Cursor.moveCursor
                            (keyDown Keyboard.Shift model)
                            ( Units.asciiUnit 0, Units.asciiUnit 1 )
                            model.cursor
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Just Keyboard.Backspace ->
            if cursorEnabled model then
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

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


mouseUp : Point2d Pixels ScreenCoordinate -> { a | start : Point2d Pixels ScreenCoordinate } -> FrontendLoaded -> FrontendLoaded
mouseUp mousePosition mouseState model =
    let
        isSmallDistance =
            Vector2d.from mouseState.start mousePosition
                |> Vector2d.length
                |> Quantity.lessThan (Pixels.pixels 5)

        model_ =
            { model
                | mouseLeft = MouseButtonUp
                , viewPoint =
                    case ( model.mouseMiddle, model.tool ) of
                        ( MouseButtonUp, DragTool ) ->
                            offsetViewPoint model mouseState.start mousePosition

                        ( MouseButtonUp, HideUserTool _ ) ->
                            offsetViewPoint model mouseState.start mousePosition

                        _ ->
                            model.viewPoint
                , cursor =
                    if not (cursorEnabled model) then
                        model.cursor

                    else if isSmallDistance then
                        screenToWorld model mousePosition |> Units.worldToAscii |> Cursor.setCursor

                    else
                        model.cursor
            }
    in
    case model_.tool of
        HideUserTool (Just ( userId, hidePoint )) ->
            if isSmallDistance then
                updateLocalModel (Change.LocalHideUser userId hidePoint) model_

            else
                model_

        _ ->
            model_


resetTouchMove : FrontendLoaded -> FrontendLoaded
resetTouchMove model =
    case model.mouseLeft of
        MouseButtonUp ->
            model

        MouseButtonDown mouseState ->
            if isTouchDevice model then
                mouseUp mouseState.current mouseState model

            else
                model


copyText : FrontendLoaded -> ( FrontendLoaded, Cmd FrontendMsg )
copyText model =
    let
        model_ =
            resetTouchMove model

        localModel =
            LocalGrid.localModel model_.localModel
    in
    ( model_
    , localModel.grid
        |> selectionToString (Cursor.bounds model_.cursor) localModel.hiddenUsers localModel.adminHiddenUsers
        |> supermario_copy_to_clipboard_to_js
    )


cutText : FrontendLoaded -> ( FrontendLoaded, Cmd FrontendMsg )
cutText model =
    let
        model_ =
            resetTouchMove model

        bounds =
            Cursor.bounds model_.cursor

        localModel =
            LocalGrid.localModel model_.localModel
    in
    ( clearTextSelection bounds model_
    , localModel.grid
        |> selectionToString bounds localModel.hiddenUsers localModel.adminHiddenUsers
        |> supermario_copy_to_clipboard_to_js
    )


updateLocalModel : Change.LocalChange -> FrontendLoaded -> FrontendLoaded
updateLocalModel msg model =
    { model
        | pendingChanges = model.pendingChanges ++ [ msg ]
        , localModel = LocalGrid.update model.time (LocalChange msg) model.localModel
    }


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


selectionToString : { min : Coord AsciiUnit, max : Coord AsciiUnit } -> EverySet UserId -> EverySet UserId -> Grid -> String
selectionToString bounds hiddenUsers hiddenUsersForAll grid =
    let
        minCell =
            Grid.asciiToCellAndLocalCoord bounds.min |> Tuple.first

        maxCell =
            Grid.asciiToCellAndLocalCoord bounds.max |> Tuple.first

        flattenedCells =
            Bounds.coordRangeFold
                (\coord dict ->
                    case Grid.getCell coord grid of
                        Just cell ->
                            Dict.insert
                                (Helper.toRawCoord coord)
                                (GridCell.flatten hiddenUsers hiddenUsersForAll cell)
                                dict

                        Nothing ->
                            dict
                )
                identity
                (Bounds.bounds minCell maxCell)
                Dict.empty
    in
    Bounds.coordRangeFoldReverse
        (\coord chars ->
            let
                ( cellCoord, localCoord ) =
                    Grid.asciiToCellAndLocalCoord coord
            in
            (Dict.get (Helper.toRawCoord cellCoord) flattenedCells
                |> Maybe.andThen (Array.get localCoord >> Maybe.map Tuple.second)
                |> Maybe.withDefault Ascii.default
                |> Ascii.toChar
            )
                :: chars
        )
        ((::) '\n')
        (Bounds.bounds
            bounds.min
            (bounds.max |> Helper.minusTuple ( Units.asciiUnit 1, Units.asciiUnit 1 ))
        )
        []
        |> String.fromList


selectionPoint : Coord AsciiUnit -> EverySet UserId -> EverySet UserId -> Grid -> ( Maybe UserId, Ascii )
selectionPoint position hiddenUsers hiddenUsersForAll grid =
    let
        ( cellPosition, localPosition ) =
            Grid.asciiToCellAndLocalCoord position
    in
    Grid.getCell cellPosition grid
        |> Maybe.andThen (GridCell.flatten hiddenUsers hiddenUsersForAll >> Array.get localPosition)
        |> Maybe.withDefault ( Nothing, Ascii.default )


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


maxStringLength : number
maxStringLength =
    if Env.isProduction then
        5000

    else
        100000


changeText : String -> FrontendLoaded -> FrontendLoaded
changeText text model =
    String.left maxStringLength text
        |> String.filter ((/=) '\u{000D}')
        |> String.split "\n"
        |> List.Nonempty.fromList
        |> Maybe.map (List.Nonempty.map (String.toList >> List.map (Ascii.fromChar >> Maybe.withDefault Ascii.default)))
        |> Maybe.map
            (\lines ->
                let
                    model_ =
                        if Duration.from model.undoAddLast model.time |> Quantity.greaterThan (Duration.seconds 2) then
                            updateLocalModel Change.LocalAddUndo { model | undoAddLast = model.time }

                        else
                            model
                in
                Grid.textToChange (Cursor.position model_.cursor) lines
                    |> List.Nonempty.map Change.LocalGridChange
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
            LocalGrid.localModel oldModel.localModel |> .grid |> Grid.allCellsDict

        showHighlighted : { a | userHoverHighlighted : Maybe b, userPressHighlighted : Maybe b } -> EverySet b -> EverySet b
        showHighlighted model hidden =
            EverySet.diff
                hidden
                ([ model.userHoverHighlighted, model.userPressHighlighted ]
                    |> List.filterMap identity
                    |> EverySet.fromList
                )

        oldHidden =
            LocalGrid.localModel oldModel.localModel |> .hiddenUsers |> showHighlighted oldModel

        oldHiddenForAll =
            LocalGrid.localModel oldModel.localModel |> .adminHiddenUsers |> showHighlighted oldModel

        newCells =
            LocalGrid.localModel newModel.localModel |> .grid |> Grid.allCellsDict

        newHidden =
            LocalGrid.localModel newModel.localModel |> .hiddenUsers |> showHighlighted newModel

        newHiddenForAll =
            LocalGrid.localModel newModel.localModel |> .adminHiddenUsers |> showHighlighted newModel

        newMesh newCell coord =
            Grid.mesh
                (Helper.fromRawCoord coord)
                (GridCell.flatten newHidden newHiddenForAll newCell |> Array.toList)

        hiddenUnchanged =
            oldHidden == newHidden && oldHiddenForAll == newHiddenForAll

        hiddenChanges =
            EverySet.union (EverySet.diff newHidden oldHidden) (EverySet.diff oldHidden newHidden)
                |> EverySet.union (EverySet.diff newHiddenForAll oldHiddenForAll)
                |> EverySet.union (EverySet.diff oldHiddenForAll newHiddenForAll)
                |> EverySet.toList
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
                                        if hiddenUnchanged then
                                            mesh

                                        else if List.any (\userId -> GridCell.hasChangesBy userId newCell) hiddenChanges then
                                            newMesh newCell coord

                                        else
                                            mesh

                                    Nothing ->
                                        newMesh newCell coord

                            else
                                newMesh newCell coord

                        Nothing ->
                            newMesh newCell coord
                )
                newCells
    }


viewBoundsUpdate : ( FrontendLoaded, Cmd FrontendMsg ) -> ( FrontendLoaded, Cmd FrontendMsg )
viewBoundsUpdate ( model, cmd ) =
    let
        { minX, minY, maxX, maxY } =
            viewBoundingBox model |> BoundingBox2d.extrema

        min_ =
            Point2d.xy minX minY |> Units.worldToAscii |> Grid.asciiToCellAndLocalCoord |> Tuple.first

        max_ =
            Point2d.xy maxX maxY
                |> Units.worldToAscii
                |> Grid.asciiToCellAndLocalCoord
                |> Tuple.first
                |> Helper.addTuple ( Units.cellUnit 1, Units.cellUnit 1 )

        bounds =
            Bounds.bounds min_ max_

        newBounds =
            Bounds.expand (Units.cellUnit 1) bounds
    in
    if LocalGrid.localModel model.localModel |> .viewBounds |> Bounds.containsBounds bounds then
        ( model, cmd )

    else
        ( { model
            | localModel =
                LocalGrid.update
                    model.time
                    (ClientChange (Change.ViewBoundsChange newBounds []))
                    model.localModel
          }
        , Cmd.batch [ cmd, Lamdera.sendToBackend (ChangeViewBounds newBounds) ]
        )


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
            case model.tool of
                DragTool ->
                    offsetViewPoint model start current

                HideUserTool _ ->
                    offsetViewPoint model start current

                SelectTool ->
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


updateLoadedFromBackend : ToFrontend -> FrontendLoaded -> ( FrontendLoaded, Cmd FrontendMsg )
updateLoadedFromBackend msg model =
    case msg of
        LoadingData _ ->
            ( model, Cmd.none )

        ChangeBroadcast changes ->
            ( { model
                | localModel = LocalGrid.updateFromBackend changes model.localModel
              }
            , Cmd.none
            )


textarea : FrontendLoaded -> Element.Attribute FrontendMsg
textarea model =
    if cursorEnabled model then
        Html.textarea
            [ Html.Events.onInput UserTyped
            , Html.Attributes.value ""
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "height" "100%"
            , Html.Attributes.style "resize" "none"
            , Html.Attributes.style "opacity" "0"
            , Html.Attributes.id "textareaId"
            , Html.Events.Extra.Touch.onWithOptions
                "touchmove"
                { stopPropagation = False, preventDefault = True }
                (\event ->
                    case event.touches of
                        head :: _ ->
                            let
                                ( x, y ) =
                                    head.pagePos
                            in
                            TouchMove (Point2d.pixels x y)

                        [] ->
                            NoOpFrontendMsg
                )
            , Html.Events.Extra.Mouse.onDown
                (\{ clientPos, button } ->
                    MouseDown button (Point2d.pixels (Tuple.first clientPos) (Tuple.second clientPos))
                )
            ]
            []
            |> Element.html
            |> Element.inFront

    else
        Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.htmlAttribute <|
                Html.Events.Extra.Mouse.onDown
                    (\{ clientPos, button } ->
                        MouseDown button (Point2d.pixels (Tuple.first clientPos) (Tuple.second clientPos))
                    )
            ]
            Element.none
            |> Element.inFront


lostConnection : FrontendLoaded -> Bool
lostConnection model =
    case LocalModel.localMsgs model.localModel of
        ( time, _ ) :: _ ->
            Duration.from time model.time |> Quantity.greaterThan (Duration.seconds 10)

        [] ->
            False


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title =
        case model of
            Loading _ ->
                "Ascii Collab"

            Loaded loadedModel ->
                if lostConnection loadedModel then
                    "Ascii Collab (offline)"

                else
                    "Ascii Collab"
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
                    (Element.width Element.fill
                        :: Element.height Element.fill
                        :: Element.clip
                        :: textarea loadedModel
                        :: Element.inFront (toolbarView loadedModel)
                        :: Element.inFront (userListView loadedModel)
                        :: (case ( loadedModel.mouseLeft, loadedModel.mouseMiddle, loadedModel.tool ) of
                                ( MouseButtonDown _, _, _ ) ->
                                    mouseAttributes

                                ( _, MouseButtonDown _, _ ) ->
                                    mouseAttributes

                                ( _, _, HideUserTool _ ) ->
                                    mouseAttributes

                                _ ->
                                    []
                           )
                        ++ (case loadedModel.tool of
                                HideUserTool (Just ( hideUserId, _ )) ->
                                    if
                                        LocalGrid.localModel loadedModel.localModel
                                            |> .user
                                            |> Tuple.first
                                            |> (==) hideUserId
                                    then
                                        []

                                    else
                                        [ Element.pointer ]

                                _ ->
                                    []
                           )
                    )
                    (Element.html (canvasView loadedModel))
        ]
    }


offlineWarningView : Element msg
offlineWarningView =
    Element.text "⚠ Unable to reach server. Your changes won't be saved."
        |> Element.el
            [ Element.Background.color UiColors.warning
            , Element.padding 8
            , Element.Border.rounded 4
            , Element.centerX
            , Element.moveUp 8
            ]


mouseAttributes : List (Element.Attribute FrontendMsg)
mouseAttributes =
    [ Html.Events.Extra.Mouse.onMove
        (\{ clientPos } ->
            MouseMove (Point2d.pixels (Tuple.first clientPos) (Tuple.second clientPos))
        )
        |> Element.htmlAttribute
    , Html.Events.Extra.Mouse.onUp
        (\{ clientPos, button } ->
            MouseUp button (Point2d.pixels (Tuple.first clientPos) (Tuple.second clientPos))
        )
        |> Element.htmlAttribute
    ]


isAdmin : FrontendLoaded -> Bool
isAdmin model =
    currentUserId model |> Just |> (==) Env.adminUserId |> (&&) model.adminEnabled


currentUserId : FrontendLoaded -> UserId
currentUserId =
    .localModel >> LocalGrid.localModel >> .user >> Tuple.first


userListView : FrontendLoaded -> Element FrontendMsg
userListView model =
    let
        localModel =
            LocalGrid.localModel model.localModel

        colorSquare isFirst isLast ( userId, userData ) =
            Element.Input.button
                (Element.padding 4
                    :: Element.Border.widthEach
                        { left = 0
                        , right = 1
                        , top =
                            if isFirst then
                                0

                            else
                                1
                        , bottom =
                            if isLast then
                                0

                            else
                                1
                        }
                    :: Element.Events.onMouseEnter (UserTagMouseEntered userId)
                    :: Element.Events.onMouseLeave (UserTagMouseExited userId)
                    :: buttonAttributes (isActive userId)
                )
                { onPress = Just (UserColorSquarePressed userId)
                , label = colorSquareInner ( userId, userData )
                }

        colorSquareInner : ( UserId, UserData ) -> Element FrontendMsg
        colorSquareInner ( userId, userData ) =
            Element.el
                [ Element.width (Element.px 20)
                , Element.height (Element.px 20)
                , Element.Border.rounded 2
                , Element.Border.width 1
                , Element.Border.color UiColors.colorSquareBorder
                , Element.Background.color <| ColorIndex.toColor <| User.color userData
                ]
                (if isAdmin model then
                    Element.paragraph
                        [ Element.Font.size 9, Element.spacing 0, Element.moveDown 1, Element.moveRight 1 ]
                        [ User.rawId userId |> String.fromInt |> Element.text ]

                 else
                    Element.none
                )

        youText =
            if isAdmin model then
                Element.el
                    [ Element.Font.bold, Element.centerX, Element.Font.color UiColors.adminText ]
                    (Element.text "⇽ Admin")

            else
                Element.el [ Element.Font.bold, Element.centerX ] (Element.text "⇽ You")

        userTag : Element FrontendMsg
        userTag =
            baseTag
                True
                (List.isEmpty hiddenUsers && not showHiddenUsersForAll)
                (if Just (currentUserId model) == Env.adminUserId then
                    Element.Input.button
                        [ Element.width Element.fill, Element.height Element.fill ]
                        { onPress = Just ToggleAdminEnabledPressed
                        , label = youText
                        }

                 else
                    youText
                )
                localModel.user

        baseTag : Bool -> Bool -> Element FrontendMsg -> ( UserId, UserData ) -> Element FrontendMsg
        baseTag isFirst isLast content ( userId, userData ) =
            Element.row
                [ Element.width Element.fill
                , "User Id: "
                    ++ String.fromInt (User.rawId userId)
                    |> Element.text
                    |> Element.el [ Element.htmlAttribute <| Html.Attributes.style "visibility" "collapse" ]
                    |> Element.behindContent
                ]
                [ colorSquare isFirst isLast ( userId, userData )
                , content
                ]

        rowBorderWidth : Bool -> Bool -> List (Element.Attribute msg)
        rowBorderWidth isFirst isLast =
            Element.Border.widthEach
                { left = 0
                , right = 0
                , top =
                    if isFirst then
                        0

                    else
                        1
                , bottom =
                    if isLast then
                        0

                    else
                        1
                }
                :: (if isLast then
                        [ Element.Border.roundEach { bottomLeft = 1, topLeft = 0, topRight = 0, bottomRight = 0 } ]

                    else
                        []
                   )

        hiddenUserTag : Bool -> Bool -> ( UserId, UserData ) -> Element FrontendMsg
        hiddenUserTag isFirst isLast ( userId, userData ) =
            Element.Input.button
                (Element.Events.onMouseEnter (UserTagMouseEntered userId)
                    :: Element.Events.onMouseLeave (UserTagMouseExited userId)
                    :: Element.width Element.fill
                    :: Element.padding 4
                    :: rowBorderWidth isFirst isLast
                    ++ buttonAttributes (isActive userId)
                )
                { onPress = Just (UnhideUserPressed userId)
                , label =
                    Element.row [ Element.width Element.fill ]
                        [ colorSquareInner ( userId, userData )
                        , Element.el [ Element.centerX ] (Element.text "Unhide")
                        ]
                }
                |> (\a ->
                        if isAdmin model then
                            Element.row [ Element.width Element.fill ]
                                [ a
                                , Element.Input.button
                                    (Element.Border.color UiColors.border
                                        :: Element.Background.color UiColors.button
                                        :: Element.mouseOver [ Element.Background.color UiColors.buttonActive ]
                                        :: Element.height Element.fill
                                        :: Element.width Element.fill
                                        :: rowBorderWidth isFirst isLast
                                    )
                                    { onPress = Just (HideForAllTogglePressed userId)
                                    , label = Element.el [ Element.centerX ] (Element.text "Hide for all")
                                    }
                                ]

                        else
                            a
                   )

        hiddenUserForAllTag : Bool -> Bool -> ( UserId, UserData ) -> Element FrontendMsg
        hiddenUserForAllTag isFirst isLast ( userId, userData ) =
            Element.Input.button
                (Element.Events.onMouseEnter (UserTagMouseEntered userId)
                    :: Element.Events.onMouseLeave (UserTagMouseExited userId)
                    :: Element.width Element.fill
                    :: Element.padding 4
                    :: rowBorderWidth isFirst isLast
                    ++ buttonAttributes (isActive userId)
                )
                { onPress = Just (HideForAllTogglePressed userId)
                , label =
                    Element.row [ Element.width Element.fill ]
                        [ colorSquareInner ( userId, userData )
                        , Element.el [ Element.centerX ] (Element.text "Unhide for all")
                        ]
                }

        buttonAttributes isActive_ =
            [ Element.Border.color UiColors.border
            , Element.Background.color
                (if isActive_ then
                    UiColors.buttonActive

                 else
                    UiColors.button
                )
            ]

        hiddenUserList : List ( UserId, UserData )
        hiddenUserList =
            EverySet.diff localModel.hiddenUsers localModel.adminHiddenUsers
                |> EverySet.toList
                |> List.filterMap (\userId -> List.find (Tuple.first >> (==) userId) localModel.otherUsers)

        isActive : UserId -> Bool
        isActive userId =
            (model.userPressHighlighted == Just userId)
                || (model.userHoverHighlighted == Just userId)
                || (case model.tool of
                        HideUserTool (Just ( hideUserId, _ )) ->
                            hideUserId == userId

                        _ ->
                            False
                   )

        hiddenUsers : List (Element FrontendMsg)
        hiddenUsers =
            hiddenUserList
                |> List.indexedMap
                    (\index otherUser ->
                        hiddenUserTag
                            False
                            (List.length hiddenUserList - 1 == index && not showHiddenUsersForAll)
                            otherUser
                    )

        hiddenusersForAllList : List ( UserId, UserData )
        hiddenusersForAllList =
            EverySet.toList localModel.adminHiddenUsers
                |> List.filterMap (\userId -> List.find (Tuple.first >> (==) userId) localModel.otherUsers)

        hiddenUsersForAll : List (Element FrontendMsg)
        hiddenUsersForAll =
            hiddenusersForAllList
                |> List.indexedMap
                    (\index otherUser ->
                        hiddenUserForAllTag
                            False
                            (List.length hiddenusersForAllList - 1 == index)
                            otherUser
                    )

        showHiddenUsersForAll =
            not (List.isEmpty hiddenUsersForAll) && isAdmin model
    in
    Element.column
        [ Element.Background.color UiColors.background
        , Element.alignRight
        , Element.spacing 8
        , Element.Border.widthEach { bottom = 1, left = 1, right = 1, top = 0 }
        , Element.Border.roundEach { bottomLeft = 3, topLeft = 0, topRight = 0, bottomRight = 0 }
        , Element.Border.color UiColors.border
        , Element.Font.color UiColors.text
        , if isAdmin model then
            Element.width (Element.px 230)

          else
            Element.width (Element.px 130)
        ]
        [ userTag
        , if List.isEmpty hiddenUsers && not showHiddenUsersForAll then
            Element.none

          else
            Element.column
                [ Element.width Element.fill, Element.spacing 4 ]
                [ Element.el [ Element.paddingXY 8 0 ] (Element.text "Hidden")
                , Element.column
                    [ Element.width Element.fill, Element.spacing 2 ]
                    hiddenUsers
                ]
        , if showHiddenUsersForAll then
            Element.column
                [ Element.width Element.fill, Element.spacing 4 ]
                [ Element.el [ Element.paddingXY 8 0 ] (Element.text "Hidden for all")
                , Element.column
                    [ Element.width Element.fill, Element.spacing 2 ]
                    hiddenUsersForAll
                ]

          else
            Element.none
        ]


canUndo : FrontendLoaded -> Bool
canUndo model =
    LocalGrid.localModel model.localModel |> .undoHistory |> List.isEmpty |> not


canRedo : FrontendLoaded -> Bool
canRedo model =
    LocalGrid.localModel model.localModel |> .redoHistory |> List.isEmpty |> not


toolbarView : FrontendLoaded -> Element FrontendMsg
toolbarView model =
    let
        zoomView =
            List.range 1 3
                |> List.map
                    (\zoom ->
                        toolbarButton
                            [ if model.zoomFactor == zoom then
                                Element.Background.color UiColors.buttonActive

                              else
                                Element.Background.color UiColors.button
                            ]
                            (ZoomFactorPressed zoom)
                            True
                            (Element.el [ Element.moveDown 1 ] (Element.text (String.fromInt zoom ++ "x")))
                    )

        toolView =
            List.map
                (\( toolDefault, isTool, icon ) ->
                    toolbarButton
                        [ if isTool model.tool then
                            Element.Background.color UiColors.buttonActive

                          else
                            Element.Background.color UiColors.button
                        ]
                        (SelectToolPressed toolDefault)
                        True
                        icon
                )
                tools

        undoRedoView =
            [ toolbarButton
                []
                UndoPressed
                (canUndo model)
                (Element.image
                    [ Element.width (Element.px 22) ]
                    { src = "undo.svg", description = "Undo button" }
                )
            , toolbarButton
                []
                RedoPressed
                (canRedo model)
                (Element.image
                    [ Element.width (Element.px 22) ]
                    { src = "redo.svg", description = "Undo button" }
                )
            ]

        copyView =
            [ toolbarButton
                []
                CopyPressed
                (cursorEnabled model)
                (Element.image
                    [ Element.width (Element.px 22) ]
                    { src = "copy.svg", description = "Copy text button" }
                )
            , toolbarButton
                []
                CutPressed
                (cursorEnabled model)
                (Element.image
                    [ Element.width (Element.px 22) ]
                    { src = "cut.svg", description = "Cut text button" }
                )
            ]

        groups =
            [ Element.el [ Element.paddingXY 4 0 ] (Element.text "🔎") :: zoomView
            , toolView
            , undoRedoView
            , copyView
            ]
                |> List.map (Element.row [ Element.spacing 2 ])
    in
    Element.wrappedRow
        [ Element.Background.color UiColors.background
        , Element.spacingXY 10 8
        , Element.padding 6
        , Element.Border.color UiColors.border
        , Element.Border.width 1
        , Element.Border.rounded 3
        , Element.Font.color UiColors.text
        , Element.above
            (if lostConnection model then
                offlineWarningView

             else
                Element.none
            )
        ]
        groups
        |> Element.el
            [ Element.paddingXY 8 0
            , Element.alignBottom
            , Element.centerX
            , Element.moveUp 8
            ]


tools : List ( ToolType, ToolType -> Bool, Element msg )
tools =
    [ ( DragTool
      , (==) DragTool
      , Element.image
            [ Element.width (Element.px 22) ]
            { src = "4-direction-arrows.svg", description = "Drag tool" }
      )
    , ( SelectTool
      , (==) SelectTool
      , Element.el
            [ Element.Border.width 2
            , Element.Border.dashed
            , Element.width (Element.px 22)
            , Element.height (Element.px 22)
            ]
            Element.none
      )
    , ( HideUserTool Nothing
      , \tool ->
            case tool of
                HideUserTool _ ->
                    True

                _ ->
                    False
      , Element.el [ Element.Font.size 24, Element.moveDown 1 ] (Element.text "🙈")
      )
    ]


toolbarButton : List (Element.Attribute msg) -> msg -> Bool -> Element msg -> Element msg
toolbarButton attributes onPress isEnabled label =
    Element.Input.button
        (Element.Background.color UiColors.button
            :: Element.width (Element.px 40)
            :: Element.height (Element.px 40)
            :: Element.mouseOver
                (if isEnabled then
                    [ Element.Background.color UiColors.buttonActive ]

                 else
                    []
                )
            :: Element.Border.width 1
            :: Element.Border.rounded 2
            :: Element.Border.color UiColors.border
            :: attributes
        )
        { onPress = Just onPress
        , label =
            Element.el
                [ if isEnabled then
                    Element.alpha 1

                  else
                    Element.alpha 0.5
                , Element.centerX
                , Element.centerY
                ]
                label
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


viewBoundingBox : FrontendLoaded -> BoundingBox2d WorldPixel WorldCoordinate
viewBoundingBox model =
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
    in
    BoundingBox2d.from viewMin viewMax


canvasView : FrontendLoaded -> Html FrontendMsg
canvasView model =
    let
        viewBounds_ =
            viewBoundingBox model

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

        color =
            LocalGrid.localModel model.localModel |> .user |> Tuple.second |> User.color |> ColorIndex.toColor

        localModel =
            LocalGrid.localModel model.localModel

        allUsers =
            localModel.user :: localModel.otherUsers
    in
    WebGL.toHtmlWith
        [ WebGL.alpha False, WebGL.antialias ]
        [ Html.Attributes.width windowWidth
        , Html.Attributes.height windowHeight
        , Html.Attributes.style "width" (String.fromInt cssWindowWidth ++ "px")
        , Html.Attributes.style "height" (String.fromInt cssWindowHeight ++ "px")
        ]
        ((if cursorEnabled model then
            [ Cursor.draw viewMatrix color model ]

          else
            []
         )
            ++ (Maybe.map
                    (drawText
                        (Dict.filter
                            (\key _ ->
                                Helper.fromRawCoord key
                                    |> Units.cellToAscii
                                    |> Units.asciiToWorld
                                    |> Helper.coordToPoint
                                    |> (\p -> BoundingBox2d.contains p viewBounds_)
                            )
                            model.meshes
                        )
                        (Maybe.andThen (\userId -> List.find (Tuple.first >> (==) userId) allUsers) model.userPressHighlighted)
                        (case ( model.tool, model.userHoverHighlighted ) of
                            ( _, Just userId ) ->
                                List.find (Tuple.first >> (==) userId) allUsers

                            ( HideUserTool (Just ( userId, _ )), _ ) ->
                                List.find (Tuple.first >> (==) userId) allUsers

                            _ ->
                                Nothing
                        )
                        viewMatrix
                    )
                    model.texture
                    |> Maybe.withDefault []
               )
        )


drawText :
    Dict ( Int, Int ) (WebGL.Mesh Grid.Vertex)
    -> Maybe ( UserId, UserData )
    -> Maybe ( UserId, UserData )
    -> Mat4
    -> Texture
    -> List WebGL.Entity
drawText meshes userPressHighlighted userHoverHighlighted viewMatrix texture =
    let
        hover =
            if userPressHighlighted == userHoverHighlighted then
                Nothing

            else
                userHoverHighlighted
    in
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
                    , highlightColor0 =
                        userPressHighlighted
                            |> Maybe.map (Tuple.second >> User.color >> ColorIndex.toColor >> ColorIndex.colorToVec3)
                            |> Maybe.withDefault (Math.Vector3.vec3 0 0 0)
                    , highlightedUser0 =
                        userPressHighlighted
                            |> Maybe.map (Tuple.first >> User.rawId >> toFloat)
                            |> Maybe.withDefault -2
                    , highlightColor1 =
                        hover
                            |> Maybe.map (Tuple.second >> User.color >> ColorIndex.toColor >> ColorIndex.colorToVec3)
                            |> Maybe.withDefault (Math.Vector3.vec3 0 0 0)
                    , highlightedUser1 =
                        hover
                            |> Maybe.map (Tuple.first >> User.rawId >> toFloat)
                            |> Maybe.withDefault -2
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
                    , Time.every 1000 ShortIntervalElapsed
                    , if loadedModel.mouseLeft /= MouseButtonUp && isTouchDevice loadedModel then
                        Time.every 100 VeryShortIntervalElapsed

                      else
                        Sub.none
                    ]
        ]


vertexShader : Shader Grid.Vertex { u | view : Mat4, highlightedUser0 : Float, highlightedUser1 : Float } { vcoord : Vec2, highlight : Float }
vertexShader =
    [glsl|

attribute vec2 position;
attribute vec2 texturePosition;
attribute float userId;
uniform mat4 view;
uniform float highlightedUser0;
uniform float highlightedUser1;
varying vec2 vcoord;
varying float highlight;

void main () {
    gl_Position = view * vec4(position, 0.0, 1.0);
    vcoord = texturePosition;
    highlight = float(userId == highlightedUser0) + float(userId == highlightedUser1) * 2.0;
}

|]


fragmentShader : Shader {} { u | texture : Texture, highlightColor0 : Vec3, highlightColor1 : Vec3 } { vcoord : Vec2, highlight : Float }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        uniform vec3 highlightColor0;
        uniform vec3 highlightColor1;
        varying vec2 vcoord;
        varying float highlight;
        void main () {
            vec4 textureColor = texture2D(texture, vcoord);

            vec4 textColor =
                float(highlight == 0.0) * vec4(0.0,0.0,0.0,1.0)
                    + float(highlight == 1.0) * vec4(highlightColor0 / 2.0,1.0)
                    + float(highlight == 2.0) * vec4(highlightColor1 / 2.0,1.0);
            vec4 backColor =
                float(highlight == 0.0) * vec4(0.0,0.0,0.0,0.0)
                    + float(highlight == 1.0) * vec4(highlightColor0,1.0)
                    + float(highlight == 2.0) * vec4(highlightColor1,1.0);

            gl_FragColor =
                float(textureColor.x == 1.0) * textColor
                    + float(textureColor.x == 0.0) * backColor;
        }
    |]
