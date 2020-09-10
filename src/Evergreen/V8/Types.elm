module Evergreen.V8.Types exposing (..)

import Evergreen.V8.Bounds
import Browser
import Browser.Navigation
import Evergreen.V8.Change
import Evergreen.V8.Cursor
import Dict
import EverySet
import Evergreen.V8.Grid
import Evergreen.V8.Helper
import Html.Events.Extra.Mouse
import Keyboard
import List.Nonempty
import Evergreen.V8.LocalGrid
import Evergreen.V8.LocalModel
import Math.Vector2
import Pixels
import Evergreen.V8.Point2d
import Quantity
import Time
import Evergreen.V8.Units
import Url
import Evergreen.V8.User
import WebGL
import WebGL.Texture


type alias FrontendLoading = 
    { key : Browser.Navigation.Key
    , windowSize : (Evergreen.V8.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V8.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , time : Time.Posix
    }


type MouseButtonState
    = MouseButtonUp
    | MouseButtonDown 
    { start : (Evergreen.V8.Point2d.Point2d Pixels.Pixels Evergreen.V8.Units.ScreenCoordinate)
    , start_ : (Evergreen.V8.Point2d.Point2d Evergreen.V8.Units.WorldPixel Evergreen.V8.Units.WorldCoordinate)
    , current : (Evergreen.V8.Point2d.Point2d Pixels.Pixels Evergreen.V8.Units.ScreenCoordinate)
    }


type ToolType
    = DragTool
    | SelectTool
    | HideUserTool (Maybe Evergreen.V8.User.UserId)


type alias FrontendLoaded = 
    { key : Browser.Navigation.Key
    , localModel : (Evergreen.V8.LocalModel.LocalModel Evergreen.V8.Change.Change Evergreen.V8.LocalGrid.LocalGrid)
    , meshes : (Dict.Dict Evergreen.V8.Helper.RawCellCoord (WebGL.Mesh Evergreen.V8.Grid.Vertex))
    , cursorMesh : (WebGL.Mesh 
    { position : Math.Vector2.Vec2
    })
    , viewPoint : (Evergreen.V8.Point2d.Point2d Evergreen.V8.Units.WorldPixel Evergreen.V8.Units.WorldCoordinate)
    , viewPointLastInterval : (Evergreen.V8.Point2d.Point2d Evergreen.V8.Units.WorldPixel Evergreen.V8.Units.WorldCoordinate)
    , cursor : Evergreen.V8.Cursor.Cursor
    , texture : (Maybe WebGL.Texture.Texture)
    , pressedKeys : (List Keyboard.Key)
    , windowSize : (Evergreen.V8.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V8.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , mouseMiddle : MouseButtonState
    , pendingChanges : (List Evergreen.V8.Change.LocalChange)
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : (Maybe Time.Posix)
    , userPressHighlighted : (Maybe Evergreen.V8.User.UserId)
    , userHoverHighlighted : (Maybe Evergreen.V8.User.UserId)
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias SessionId = String


type alias ClientId = String


type alias BackendUserData = 
    { userData : Evergreen.V8.User.UserData
    , hiddenUsers : (EverySet.EverySet Evergreen.V8.User.UserId)
    , undoHistory : (List (Dict.Dict Evergreen.V8.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V8.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V8.Helper.RawCellCoord Int)
    }


type alias BackendModel =
    { grid : Evergreen.V8.Grid.Grid
    , userSessions : (Dict.Dict SessionId 
    { clientIds : (Dict.Dict ClientId (Evergreen.V8.Bounds.Bounds Evergreen.V8.Units.CellUnit))
    , userId : Evergreen.V8.User.UserId
    })
    , users : (Dict.Dict Evergreen.V8.User.RawUserId BackendUserData)
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V8.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V8.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V8.Point2d.Point2d Pixels.Pixels Evergreen.V8.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V8.Point2d.Point2d Pixels.Pixels Evergreen.V8.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V8.Point2d.Point2d Pixels.Pixels Evergreen.V8.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V8.Point2d.Point2d Pixels.Pixels Evergreen.V8.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V8.User.UserId
    | UserColorSquarePressed Evergreen.V8.User.UserId
    | UserTagMouseEntered Evergreen.V8.User.UserId
    | UserTagMouseExited Evergreen.V8.User.UserId


type ToBackend
    = RequestData (Evergreen.V8.Bounds.Bounds Evergreen.V8.Units.CellUnit)
    | GridChange (List.Nonempty.Nonempty Evergreen.V8.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V8.Bounds.Bounds Evergreen.V8.Units.CellUnit)


type BackendMsg
    = UserDisconnected SessionId ClientId


type alias LoadingData_ = 
    { user : (Evergreen.V8.User.UserId, Evergreen.V8.User.UserData)
    , grid : Evergreen.V8.Grid.Grid
    , otherUsers : (List (Evergreen.V8.User.UserId, Evergreen.V8.User.UserData))
    , hiddenUsers : (EverySet.EverySet Evergreen.V8.User.UserId)
    , undoHistory : (List (Dict.Dict Evergreen.V8.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V8.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V8.Helper.RawCellCoord Int)
    , viewBounds : (Evergreen.V8.Bounds.Bounds Evergreen.V8.Units.CellUnit)
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V8.Change.Change)