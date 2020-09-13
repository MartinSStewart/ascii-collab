module Evergreen.V14.Types exposing (..)

import Evergreen.V14.Bounds
import Browser
import Browser.Navigation
import Evergreen.V14.Change
import Evergreen.V14.Cursor
import Dict
import EverySet
import Evergreen.V14.Grid
import Evergreen.V14.Helper
import Html.Events.Extra.Mouse
import Keyboard
import List.Nonempty
import Evergreen.V14.LocalGrid
import Evergreen.V14.LocalModel
import Math.Vector2
import Pixels
import Evergreen.V14.Point2d
import Quantity
import Time
import Evergreen.V14.Units
import Url
import Evergreen.V14.User
import WebGL
import WebGL.Texture


type alias FrontendLoading = 
    { key : Browser.Navigation.Key
    , windowSize : (Evergreen.V14.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V14.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : (Evergreen.V14.Helper.Coord Evergreen.V14.Units.AsciiUnit)
    }


type MouseButtonState
    = MouseButtonUp
    | MouseButtonDown 
    { start : (Evergreen.V14.Point2d.Point2d Pixels.Pixels Evergreen.V14.Units.ScreenCoordinate)
    , start_ : (Evergreen.V14.Point2d.Point2d Evergreen.V14.Units.WorldPixel Evergreen.V14.Units.WorldCoordinate)
    , current : (Evergreen.V14.Point2d.Point2d Pixels.Pixels Evergreen.V14.Units.ScreenCoordinate)
    }


type ToolType
    = DragTool
    | SelectTool
    | HideUserTool (Maybe (Evergreen.V14.User.UserId, (Evergreen.V14.Helper.Coord Evergreen.V14.Units.AsciiUnit)))


type alias FrontendLoaded = 
    { key : Browser.Navigation.Key
    , localModel : (Evergreen.V14.LocalModel.LocalModel Evergreen.V14.Change.Change Evergreen.V14.LocalGrid.LocalGrid)
    , meshes : (Dict.Dict Evergreen.V14.Helper.RawCellCoord (WebGL.Mesh Evergreen.V14.Grid.Vertex))
    , cursorMesh : (WebGL.Mesh 
    { position : Math.Vector2.Vec2
    })
    , viewPoint : (Evergreen.V14.Point2d.Point2d Evergreen.V14.Units.WorldPixel Evergreen.V14.Units.WorldCoordinate)
    , viewPointLastInterval : (Evergreen.V14.Point2d.Point2d Evergreen.V14.Units.WorldPixel Evergreen.V14.Units.WorldCoordinate)
    , cursor : Evergreen.V14.Cursor.Cursor
    , texture : (Maybe WebGL.Texture.Texture)
    , pressedKeys : (List Keyboard.Key)
    , windowSize : (Evergreen.V14.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V14.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , mouseMiddle : MouseButtonState
    , pendingChanges : (List Evergreen.V14.Change.LocalChange)
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : (Maybe Time.Posix)
    , userPressHighlighted : (Maybe Evergreen.V14.User.UserId)
    , userHoverHighlighted : (Maybe Evergreen.V14.User.UserId)
    , adminEnabled : Bool
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias SessionId = String


type alias ClientId = String


type alias BackendUserData = 
    { userData : Evergreen.V14.User.UserData
    , hiddenUsers : (EverySet.EverySet Evergreen.V14.User.UserId)
    , hiddenForAll : Bool
    , undoHistory : (List (Dict.Dict Evergreen.V14.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V14.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V14.Helper.RawCellCoord Int)
    }


type alias BackendModel =
    { grid : Evergreen.V14.Grid.Grid
    , userSessions : (Dict.Dict SessionId 
    { clientIds : (Dict.Dict ClientId (Evergreen.V14.Bounds.Bounds Evergreen.V14.Units.CellUnit))
    , userId : Evergreen.V14.User.UserId
    })
    , users : (Dict.Dict Evergreen.V14.User.RawUserId BackendUserData)
    , usersHiddenRecently : (List 
    { reporter : Evergreen.V14.User.UserId
    , hiddenUser : Evergreen.V14.User.UserId
    , hidePoint : (Evergreen.V14.Helper.Coord Evergreen.V14.Units.AsciiUnit)
    })
    , userChangesRecently : (Dict.Dict (Evergreen.V14.User.RawUserId, Evergreen.V14.Helper.RawCellCoord) Int)
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V14.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V14.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V14.Point2d.Point2d Pixels.Pixels Evergreen.V14.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V14.Point2d.Point2d Pixels.Pixels Evergreen.V14.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V14.Point2d.Point2d Pixels.Pixels Evergreen.V14.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V14.Point2d.Point2d Pixels.Pixels Evergreen.V14.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V14.User.UserId
    | UserColorSquarePressed Evergreen.V14.User.UserId
    | UserTagMouseEntered Evergreen.V14.User.UserId
    | UserTagMouseExited Evergreen.V14.User.UserId
    | HideForAllTogglePressed Evergreen.V14.User.UserId
    | ToggleAdminEnabledPressed


type ToBackend
    = RequestData (Evergreen.V14.Bounds.Bounds Evergreen.V14.Units.CellUnit)
    | GridChange (List.Nonempty.Nonempty Evergreen.V14.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V14.Bounds.Bounds Evergreen.V14.Units.CellUnit)


type BackendMsg
    = UserDisconnected SessionId ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent


type alias LoadingData_ = 
    { user : (Evergreen.V14.User.UserId, Evergreen.V14.User.UserData)
    , grid : Evergreen.V14.Grid.Grid
    , otherUsers : (List (Evergreen.V14.User.UserId, Evergreen.V14.User.UserData))
    , hiddenUsers : (EverySet.EverySet Evergreen.V14.User.UserId)
    , adminHiddenUsers : (EverySet.EverySet Evergreen.V14.User.UserId)
    , undoHistory : (List (Dict.Dict Evergreen.V14.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V14.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V14.Helper.RawCellCoord Int)
    , viewBounds : (Evergreen.V14.Bounds.Bounds Evergreen.V14.Units.CellUnit)
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V14.Change.Change)