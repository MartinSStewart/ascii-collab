module Evergreen.V45.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Duration
import Evergreen.V45.Bounds
import Evergreen.V45.Change
import Evergreen.V45.Cursor
import Evergreen.V45.Grid
import Evergreen.V45.Helper
import Evergreen.V45.LocalGrid
import Evergreen.V45.LocalModel
import Evergreen.V45.Point2d
import Evergreen.V45.Units
import Evergreen.V45.User
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
    , windowSize : (Evergreen.V45.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V45.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : (Evergreen.V45.Helper.Coord Evergreen.V45.Units.AsciiUnit)
    , mousePosition : (Evergreen.V45.Point2d.Point2d Pixels.Pixels Evergreen.V45.Units.ScreenCoordinate)
    }


type MouseButtonState
    = MouseButtonUp 
    { current : (Evergreen.V45.Point2d.Point2d Pixels.Pixels Evergreen.V45.Units.ScreenCoordinate)
    }
    | MouseButtonDown 
    { start : (Evergreen.V45.Point2d.Point2d Pixels.Pixels Evergreen.V45.Units.ScreenCoordinate)
    , start_ : (Evergreen.V45.Point2d.Point2d Evergreen.V45.Units.WorldPixel Evergreen.V45.Units.WorldCoordinate)
    , current : (Evergreen.V45.Point2d.Point2d Pixels.Pixels Evergreen.V45.Units.ScreenCoordinate)
    }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe (Evergreen.V45.User.UserId, (Evergreen.V45.Helper.Coord Evergreen.V45.Units.AsciiUnit)))


type alias FrontendLoaded = 
    { key : Browser.Navigation.Key
    , localModel : (Evergreen.V45.LocalModel.LocalModel Evergreen.V45.Change.Change Evergreen.V45.LocalGrid.LocalGrid)
    , meshes : (Dict.Dict Evergreen.V45.Helper.RawCellCoord (WebGL.Mesh Evergreen.V45.Grid.Vertex))
    , cursorMesh : (WebGL.Mesh 
    { position : Math.Vector2.Vec2
    })
    , viewPoint : (Evergreen.V45.Point2d.Point2d Evergreen.V45.Units.WorldPixel Evergreen.V45.Units.WorldCoordinate)
    , viewPointLastInterval : (Evergreen.V45.Point2d.Point2d Evergreen.V45.Units.WorldPixel Evergreen.V45.Units.WorldCoordinate)
    , cursor : Evergreen.V45.Cursor.Cursor
    , texture : (Maybe WebGL.Texture.Texture)
    , pressedKeys : (List Keyboard.Key)
    , windowSize : (Evergreen.V45.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V45.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : (Maybe (Time.Posix, (Evergreen.V45.Point2d.Point2d Pixels.Pixels Evergreen.V45.Units.ScreenCoordinate)))
    , mouseMiddle : MouseButtonState
    , pendingChanges : (List Evergreen.V45.Change.LocalChange)
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : (Maybe Time.Posix)
    , userHoverHighlighted : (Maybe Evergreen.V45.User.UserId)
    , highlightContextMenu : (Maybe 
    { userId : Evergreen.V45.User.UserId
    , hidePoint : (Evergreen.V45.Helper.Coord Evergreen.V45.Units.AsciiUnit)
    })
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData = 
    { hiddenUsers : (EverySet.EverySet Evergreen.V45.User.UserId)
    , hiddenForAll : Bool
    , undoHistory : (List (Dict.Dict Evergreen.V45.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V45.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V45.Helper.RawCellCoord Int)
    }


type alias BackendModel =
    { grid : Evergreen.V45.Grid.Grid
    , userSessions : (Dict.Dict Lamdera.SessionId 
    { clientIds : (Dict.Dict Lamdera.ClientId (Evergreen.V45.Bounds.Bounds Evergreen.V45.Units.CellUnit))
    , userId : Evergreen.V45.User.UserId
    })
    , users : (Dict.Dict Evergreen.V45.User.RawUserId BackendUserData)
    , usersHiddenRecently : (List 
    { reporter : Evergreen.V45.User.UserId
    , hiddenUser : Evergreen.V45.User.UserId
    , hidePoint : (Evergreen.V45.Helper.Coord Evergreen.V45.Units.AsciiUnit)
    })
    , userChangesRecently : (Dict.Dict (Evergreen.V45.User.RawUserId, Evergreen.V45.Helper.RawCellCoord) Int)
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V45.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V45.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V45.Point2d.Point2d Pixels.Pixels Evergreen.V45.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V45.Point2d.Point2d Pixels.Pixels Evergreen.V45.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V45.Point2d.Point2d Pixels.Pixels Evergreen.V45.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V45.Point2d.Point2d Pixels.Pixels Evergreen.V45.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V45.User.UserId
    | UserTagMouseEntered Evergreen.V45.User.UserId
    | UserTagMouseExited Evergreen.V45.User.UserId
    | HideForAllTogglePressed Evergreen.V45.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed 
    { userId : Evergreen.V45.User.UserId
    , hidePoint : (Evergreen.V45.Helper.Coord Evergreen.V45.Units.AsciiUnit)
    }
    | AnimationFrame Time.Posix


type ToBackend
    = RequestData (Evergreen.V45.Bounds.Bounds Evergreen.V45.Units.CellUnit)
    | GridChange (List.Nonempty.Nonempty Evergreen.V45.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V45.Bounds.Bounds Evergreen.V45.Units.CellUnit)


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent


type alias LoadingData_ = 
    { user : Evergreen.V45.User.UserId
    , grid : Evergreen.V45.Grid.Grid
    , hiddenUsers : (EverySet.EverySet Evergreen.V45.User.UserId)
    , adminHiddenUsers : (EverySet.EverySet Evergreen.V45.User.UserId)
    , undoHistory : (List (Dict.Dict Evergreen.V45.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V45.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V45.Helper.RawCellCoord Int)
    , viewBounds : (Evergreen.V45.Bounds.Bounds Evergreen.V45.Units.CellUnit)
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V45.Change.Change)