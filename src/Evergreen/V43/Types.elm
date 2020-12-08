module Evergreen.V43.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Duration
import Evergreen.V43.Bounds
import Evergreen.V43.Change
import Evergreen.V43.Cursor
import Evergreen.V43.Grid
import Evergreen.V43.Helper
import Evergreen.V43.LocalGrid
import Evergreen.V43.LocalModel
import Evergreen.V43.Point2d
import Evergreen.V43.Units
import Evergreen.V43.User
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
    , windowSize : (Evergreen.V43.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V43.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : (Evergreen.V43.Helper.Coord Evergreen.V43.Units.AsciiUnit)
    , mousePosition : (Evergreen.V43.Point2d.Point2d Pixels.Pixels Evergreen.V43.Units.ScreenCoordinate)
    }


type MouseButtonState
    = MouseButtonUp
    | MouseButtonDown 
    { start : (Evergreen.V43.Point2d.Point2d Pixels.Pixels Evergreen.V43.Units.ScreenCoordinate)
    , start_ : (Evergreen.V43.Point2d.Point2d Evergreen.V43.Units.WorldPixel Evergreen.V43.Units.WorldCoordinate)
    }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe (Evergreen.V43.User.UserId, (Evergreen.V43.Helper.Coord Evergreen.V43.Units.AsciiUnit)))


type alias FrontendLoaded = 
    { key : Browser.Navigation.Key
    , localModel : (Evergreen.V43.LocalModel.LocalModel Evergreen.V43.Change.Change Evergreen.V43.LocalGrid.LocalGrid)
    , meshes : (Dict.Dict Evergreen.V43.Helper.RawCellCoord (WebGL.Mesh Evergreen.V43.Grid.Vertex))
    , cursorMesh : (WebGL.Mesh 
    { position : Math.Vector2.Vec2
    })
    , viewPoint : (Evergreen.V43.Point2d.Point2d Evergreen.V43.Units.WorldPixel Evergreen.V43.Units.WorldCoordinate)
    , viewPointLastInterval : (Evergreen.V43.Point2d.Point2d Evergreen.V43.Units.WorldPixel Evergreen.V43.Units.WorldCoordinate)
    , cursor : Evergreen.V43.Cursor.Cursor
    , texture : (Maybe WebGL.Texture.Texture)
    , pressedKeys : (List Keyboard.Key)
    , windowSize : (Evergreen.V43.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V43.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , mousePosition : (Evergreen.V43.Point2d.Point2d Pixels.Pixels Evergreen.V43.Units.ScreenCoordinate)
    , lastMouseLeftUp : (Maybe (Time.Posix, (Evergreen.V43.Point2d.Point2d Pixels.Pixels Evergreen.V43.Units.ScreenCoordinate)))
    , mouseMiddle : MouseButtonState
    , pendingChanges : (List Evergreen.V43.Change.LocalChange)
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : (Maybe Time.Posix)
    , userHoverHighlighted : (Maybe Evergreen.V43.User.UserId)
    , highlightContextMenu : (Maybe 
    { userId : Evergreen.V43.User.UserId
    , hidePoint : (Evergreen.V43.Helper.Coord Evergreen.V43.Units.AsciiUnit)
    })
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData = 
    { hiddenUsers : (EverySet.EverySet Evergreen.V43.User.UserId)
    , hiddenForAll : Bool
    , undoHistory : (List (Dict.Dict Evergreen.V43.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V43.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V43.Helper.RawCellCoord Int)
    }


type alias BackendModel =
    { grid : Evergreen.V43.Grid.Grid
    , userSessions : (Dict.Dict Lamdera.SessionId 
    { clientIds : (Dict.Dict Lamdera.ClientId (Evergreen.V43.Bounds.Bounds Evergreen.V43.Units.CellUnit))
    , userId : Evergreen.V43.User.UserId
    })
    , users : (Dict.Dict Evergreen.V43.User.RawUserId BackendUserData)
    , usersHiddenRecently : (List 
    { reporter : Evergreen.V43.User.UserId
    , hiddenUser : Evergreen.V43.User.UserId
    , hidePoint : (Evergreen.V43.Helper.Coord Evergreen.V43.Units.AsciiUnit)
    })
    , userChangesRecently : (Dict.Dict (Evergreen.V43.User.RawUserId, Evergreen.V43.Helper.RawCellCoord) Int)
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V43.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V43.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V43.Point2d.Point2d Pixels.Pixels Evergreen.V43.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V43.Point2d.Point2d Pixels.Pixels Evergreen.V43.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V43.Point2d.Point2d Pixels.Pixels Evergreen.V43.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V43.Point2d.Point2d Pixels.Pixels Evergreen.V43.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V43.User.UserId
    | UserTagMouseEntered Evergreen.V43.User.UserId
    | UserTagMouseExited Evergreen.V43.User.UserId
    | HideForAllTogglePressed Evergreen.V43.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed 
    { userId : Evergreen.V43.User.UserId
    , hidePoint : (Evergreen.V43.Helper.Coord Evergreen.V43.Units.AsciiUnit)
    }
    | AnimationFrame Time.Posix


type ToBackend
    = RequestData (Evergreen.V43.Bounds.Bounds Evergreen.V43.Units.CellUnit)
    | GridChange (List.Nonempty.Nonempty Evergreen.V43.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V43.Bounds.Bounds Evergreen.V43.Units.CellUnit)


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent


type alias LoadingData_ = 
    { user : Evergreen.V43.User.UserId
    , grid : Evergreen.V43.Grid.Grid
    , hiddenUsers : (EverySet.EverySet Evergreen.V43.User.UserId)
    , adminHiddenUsers : (EverySet.EverySet Evergreen.V43.User.UserId)
    , undoHistory : (List (Dict.Dict Evergreen.V43.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V43.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V43.Helper.RawCellCoord Int)
    , viewBounds : (Evergreen.V43.Bounds.Bounds Evergreen.V43.Units.CellUnit)
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V43.Change.Change)