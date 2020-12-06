module Evergreen.V39.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Duration
import Evergreen.V39.Bounds
import Evergreen.V39.Change
import Evergreen.V39.Cursor
import Evergreen.V39.Grid
import Evergreen.V39.Helper
import Evergreen.V39.LocalGrid
import Evergreen.V39.LocalModel
import Evergreen.V39.Point2d
import Evergreen.V39.Units
import Evergreen.V39.User
import EverySet
import Html.Events.Extra.Mouse
import Keyboard
import Lamdera
import List.Nonempty
import Math.Vector2
import Pixels
import Quantity
import Time
import Url
import WebGL
import WebGL.Texture


type alias FrontendLoading = 
    { key : Browser.Navigation.Key
    , windowSize : (Evergreen.V39.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V39.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : (Evergreen.V39.Helper.Coord Evergreen.V39.Units.AsciiUnit)
    , mousePosition : (Evergreen.V39.Point2d.Point2d Pixels.Pixels Evergreen.V39.Units.ScreenCoordinate)
    }


type MouseButtonState
    = MouseButtonUp 
    { current : (Evergreen.V39.Point2d.Point2d Pixels.Pixels Evergreen.V39.Units.ScreenCoordinate)
    }
    | MouseButtonDown 
    { start : (Evergreen.V39.Point2d.Point2d Pixels.Pixels Evergreen.V39.Units.ScreenCoordinate)
    , start_ : (Evergreen.V39.Point2d.Point2d Evergreen.V39.Units.WorldPixel Evergreen.V39.Units.WorldCoordinate)
    , current : (Evergreen.V39.Point2d.Point2d Pixels.Pixels Evergreen.V39.Units.ScreenCoordinate)
    }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe (Evergreen.V39.User.UserId, (Evergreen.V39.Helper.Coord Evergreen.V39.Units.AsciiUnit)))


type alias FrontendLoaded = 
    { key : Browser.Navigation.Key
    , localModel : (Evergreen.V39.LocalModel.LocalModel Evergreen.V39.Change.Change Evergreen.V39.LocalGrid.LocalGrid)
    , meshes : (Dict.Dict Evergreen.V39.Helper.RawCellCoord (WebGL.Mesh Evergreen.V39.Grid.Vertex))
    , cursorMesh : (WebGL.Mesh 
    { position : Math.Vector2.Vec2
    })
    , viewPoint : (Evergreen.V39.Point2d.Point2d Evergreen.V39.Units.WorldPixel Evergreen.V39.Units.WorldCoordinate)
    , viewPointLastInterval : (Evergreen.V39.Point2d.Point2d Evergreen.V39.Units.WorldPixel Evergreen.V39.Units.WorldCoordinate)
    , cursor : Evergreen.V39.Cursor.Cursor
    , texture : (Maybe WebGL.Texture.Texture)
    , pressedKeys : (List Keyboard.Key)
    , windowSize : (Evergreen.V39.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V39.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : (Maybe (Time.Posix, (Evergreen.V39.Point2d.Point2d Pixels.Pixels Evergreen.V39.Units.ScreenCoordinate)))
    , mouseMiddle : MouseButtonState
    , pendingChanges : (List Evergreen.V39.Change.LocalChange)
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : (Maybe Time.Posix)
    , userHoverHighlighted : (Maybe Evergreen.V39.User.UserId)
    , highlightContextMenu : (Maybe 
    { userId : Evergreen.V39.User.UserId
    , hidePoint : (Evergreen.V39.Helper.Coord Evergreen.V39.Units.AsciiUnit)
    })
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData = 
    { hiddenUsers : (EverySet.EverySet Evergreen.V39.User.UserId)
    , hiddenForAll : Bool
    , undoHistory : (List (Dict.Dict Evergreen.V39.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V39.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V39.Helper.RawCellCoord Int)
    }


type alias BackendModel =
    { grid : Evergreen.V39.Grid.Grid
    , userSessions : (Dict.Dict Lamdera.SessionId 
    { clientIds : (Dict.Dict Lamdera.ClientId (Evergreen.V39.Bounds.Bounds Evergreen.V39.Units.CellUnit))
    , userId : Evergreen.V39.User.UserId
    })
    , users : (Dict.Dict Evergreen.V39.User.RawUserId BackendUserData)
    , usersHiddenRecently : (List 
    { reporter : Evergreen.V39.User.UserId
    , hiddenUser : Evergreen.V39.User.UserId
    , hidePoint : (Evergreen.V39.Helper.Coord Evergreen.V39.Units.AsciiUnit)
    })
    , userChangesRecently : (Dict.Dict (Evergreen.V39.User.RawUserId, Evergreen.V39.Helper.RawCellCoord) Int)
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V39.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V39.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V39.Point2d.Point2d Pixels.Pixels Evergreen.V39.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V39.Point2d.Point2d Pixels.Pixels Evergreen.V39.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V39.Point2d.Point2d Pixels.Pixels Evergreen.V39.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V39.Point2d.Point2d Pixels.Pixels Evergreen.V39.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V39.User.UserId
    | UserTagMouseEntered Evergreen.V39.User.UserId
    | UserTagMouseExited Evergreen.V39.User.UserId
    | HideForAllTogglePressed Evergreen.V39.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed 
    { userId : Evergreen.V39.User.UserId
    , hidePoint : (Evergreen.V39.Helper.Coord Evergreen.V39.Units.AsciiUnit)
    }
    | AnimationFrame Time.Posix


type ToBackend
    = RequestData (Evergreen.V39.Bounds.Bounds Evergreen.V39.Units.CellUnit)
    | GridChange (List.Nonempty.Nonempty Evergreen.V39.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V39.Bounds.Bounds Evergreen.V39.Units.CellUnit)


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent


type alias LoadingData_ = 
    { user : Evergreen.V39.User.UserId
    , grid : Evergreen.V39.Grid.Grid
    , hiddenUsers : (EverySet.EverySet Evergreen.V39.User.UserId)
    , adminHiddenUsers : (EverySet.EverySet Evergreen.V39.User.UserId)
    , undoHistory : (List (Dict.Dict Evergreen.V39.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V39.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V39.Helper.RawCellCoord Int)
    , viewBounds : (Evergreen.V39.Bounds.Bounds Evergreen.V39.Units.CellUnit)
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V39.Change.Change)