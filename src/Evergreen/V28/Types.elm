module Evergreen.V28.Types exposing (..)

import Evergreen.V28.Bounds
import Browser
import Browser.Navigation
import Evergreen.V28.Change
import Evergreen.V28.Cursor
import Dict
import Duration
import EverySet
import Evergreen.V28.Grid
import Evergreen.V28.Helper
import Html.Events.Extra.Mouse
import Keyboard
import List.Nonempty
import Evergreen.V28.LocalGrid
import Evergreen.V28.LocalModel
import Math.Vector2
import Pixels
import Evergreen.V28.Point2d
import Quantity
import Time
import Evergreen.V28.Units
import Url
import Evergreen.V28.User
import WebGL
import WebGL.Texture


type alias FrontendLoading = 
    { key : Browser.Navigation.Key
    , windowSize : (Evergreen.V28.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V28.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : (Evergreen.V28.Helper.Coord Evergreen.V28.Units.AsciiUnit)
    }


type MouseButtonState
    = MouseButtonUp
    | MouseButtonDown 
    { start : (Evergreen.V28.Point2d.Point2d Pixels.Pixels Evergreen.V28.Units.ScreenCoordinate)
    , start_ : (Evergreen.V28.Point2d.Point2d Evergreen.V28.Units.WorldPixel Evergreen.V28.Units.WorldCoordinate)
    , current : (Evergreen.V28.Point2d.Point2d Pixels.Pixels Evergreen.V28.Units.ScreenCoordinate)
    }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe (Evergreen.V28.User.UserId, (Evergreen.V28.Helper.Coord Evergreen.V28.Units.AsciiUnit)))


type alias FrontendLoaded = 
    { key : Browser.Navigation.Key
    , localModel : (Evergreen.V28.LocalModel.LocalModel Evergreen.V28.Change.Change Evergreen.V28.LocalGrid.LocalGrid)
    , meshes : (Dict.Dict Evergreen.V28.Helper.RawCellCoord (WebGL.Mesh Evergreen.V28.Grid.Vertex))
    , cursorMesh : (WebGL.Mesh 
    { position : Math.Vector2.Vec2
    })
    , viewPoint : (Evergreen.V28.Point2d.Point2d Evergreen.V28.Units.WorldPixel Evergreen.V28.Units.WorldCoordinate)
    , viewPointLastInterval : (Evergreen.V28.Point2d.Point2d Evergreen.V28.Units.WorldPixel Evergreen.V28.Units.WorldCoordinate)
    , cursor : Evergreen.V28.Cursor.Cursor
    , texture : (Maybe WebGL.Texture.Texture)
    , pressedKeys : (List Keyboard.Key)
    , windowSize : (Evergreen.V28.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V28.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , mouseMiddle : MouseButtonState
    , pendingChanges : (List Evergreen.V28.Change.LocalChange)
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : (Maybe Time.Posix)
    , userHoverHighlighted : (Maybe Evergreen.V28.User.UserId)
    , highlightContextMenu : (Maybe 
    { userId : Evergreen.V28.User.UserId
    , hidePoint : (Evergreen.V28.Helper.Coord Evergreen.V28.Units.AsciiUnit)
    })
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias SessionId = String


type alias ClientId = String


type alias BackendUserData = 
    { hiddenUsers : (EverySet.EverySet Evergreen.V28.User.UserId)
    , hiddenForAll : Bool
    , undoHistory : (List (Dict.Dict Evergreen.V28.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V28.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V28.Helper.RawCellCoord Int)
    }


type alias BackendModel =
    { grid : Evergreen.V28.Grid.Grid
    , userSessions : (Dict.Dict SessionId 
    { clientIds : (Dict.Dict ClientId (Evergreen.V28.Bounds.Bounds Evergreen.V28.Units.CellUnit))
    , userId : Evergreen.V28.User.UserId
    })
    , users : (Dict.Dict Evergreen.V28.User.RawUserId BackendUserData)
    , usersHiddenRecently : (List 
    { reporter : Evergreen.V28.User.UserId
    , hiddenUser : Evergreen.V28.User.UserId
    , hidePoint : (Evergreen.V28.Helper.Coord Evergreen.V28.Units.AsciiUnit)
    })
    , userChangesRecently : (Dict.Dict (Evergreen.V28.User.RawUserId, Evergreen.V28.Helper.RawCellCoord) Int)
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V28.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V28.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V28.Point2d.Point2d Pixels.Pixels Evergreen.V28.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V28.Point2d.Point2d Pixels.Pixels Evergreen.V28.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V28.Point2d.Point2d Pixels.Pixels Evergreen.V28.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V28.Point2d.Point2d Pixels.Pixels Evergreen.V28.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V28.User.UserId
    | UserTagMouseEntered Evergreen.V28.User.UserId
    | UserTagMouseExited Evergreen.V28.User.UserId
    | HideForAllTogglePressed Evergreen.V28.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed 
    { userId : Evergreen.V28.User.UserId
    , hidePoint : (Evergreen.V28.Helper.Coord Evergreen.V28.Units.AsciiUnit)
    }
    | AnimationFrame Time.Posix


type ToBackend
    = RequestData (Evergreen.V28.Bounds.Bounds Evergreen.V28.Units.CellUnit)
    | GridChange (List.Nonempty.Nonempty Evergreen.V28.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V28.Bounds.Bounds Evergreen.V28.Units.CellUnit)


type BackendMsg
    = UserDisconnected SessionId ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent


type alias LoadingData_ = 
    { user : Evergreen.V28.User.UserId
    , grid : Evergreen.V28.Grid.Grid
    , hiddenUsers : (EverySet.EverySet Evergreen.V28.User.UserId)
    , adminHiddenUsers : (EverySet.EverySet Evergreen.V28.User.UserId)
    , undoHistory : (List (Dict.Dict Evergreen.V28.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V28.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V28.Helper.RawCellCoord Int)
    , viewBounds : (Evergreen.V28.Bounds.Bounds Evergreen.V28.Units.CellUnit)
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V28.Change.Change)