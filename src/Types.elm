module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , BackendUserData
    , FrontendLoaded
    , FrontendLoading
    , FrontendModel(..)
    , FrontendMsg(..)
    , LoadingData_
    , MouseButtonState(..)
    , ToBackend(..)
    , ToFrontend(..)
    , ToolType(..)
    )

import Bounds exposing (Bounds)
import Browser exposing (UrlRequest)
import Browser.Navigation
import Change exposing (Change, ServerChange)
import Cursor exposing (Cursor)
import Dict exposing (Dict)
import Duration exposing (Duration)
import EverySet exposing (EverySet)
import Grid exposing (Grid)
import Helper exposing (Coord, RawCellCoord)
import Html.Events.Extra.Mouse exposing (Button)
import Keyboard
import Lamdera exposing (ClientId, SessionId)
import List.Nonempty exposing (Nonempty)
import LocalGrid exposing (LocalGrid)
import LocalModel exposing (LocalModel)
import Math.Vector2 exposing (Vec2)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Time
import Units exposing (AsciiUnit, CellUnit, ScreenCoordinate, WorldCoordinate, WorldPixel)
import Url exposing (Url)
import User exposing (RawUserId, UserId)
import WebGL
import WebGL.Texture exposing (Texture)


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendLoading =
    { key : Browser.Navigation.Key
    , windowSize : Coord Pixels
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Coord AsciiUnit
    , mousePosition : Point2d Pixels ScreenCoordinate
    }


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : LocalModel Change LocalGrid
    , meshes : Dict RawCellCoord (WebGL.Mesh Grid.Vertex)
    , cursorMesh : WebGL.Mesh { position : Vec2 }
    , viewPoint : Point2d WorldPixel WorldCoordinate
    , viewPointLastInterval : Point2d WorldPixel WorldCoordinate
    , cursor : Cursor
    , texture : Maybe Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Coord Pixels
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : Maybe ( Time.Posix, Point2d Pixels ScreenCoordinate )
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userHoverHighlighted : Maybe UserId
    , highlightContextMenu : Maybe { userId : UserId, hidePoint : Coord AsciiUnit }
    , adminEnabled : Bool
    , animationElapsedTime : Duration
    , ignoreNextUrlChanged : Bool
    }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe ( UserId, Coord AsciiUnit ))


type MouseButtonState
    = MouseButtonUp { current : Point2d Pixels ScreenCoordinate }
    | MouseButtonDown
        { start : Point2d Pixels ScreenCoordinate
        , start_ : Point2d WorldPixel WorldCoordinate
        , current : Point2d Pixels ScreenCoordinate
        }


type alias BackendModel =
    { grid : Grid
    , userSessions : Dict SessionId { clientIds : Dict ClientId (Bounds CellUnit), userId : UserId }
    , users : Dict RawUserId BackendUserData
    , usersHiddenRecently : List { reporter : UserId, hiddenUser : UserId, hidePoint : Coord AsciiUnit }
    , userChangesRecently : Dict ( RawUserId, RawCellCoord ) Int
    }


type alias BackendUserData =
    { hiddenUsers : EverySet UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict RawCellCoord Int)
    , redoHistory : List (Dict RawCellCoord Int)
    , undoCurrent : Dict RawCellCoord Int
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Coord Pixels)
    | GotDevicePixelRatio (Quantity Float (Rate WorldPixel Pixels))
    | UserTyped String
    | MouseDown Button (Point2d Pixels ScreenCoordinate)
    | MouseUp Button (Point2d Pixels ScreenCoordinate)
    | MouseMove (Point2d Pixels ScreenCoordinate)
    | TouchMove (Point2d Pixels ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed UserId
    | UserTagMouseEntered UserId
    | UserTagMouseExited UserId
    | HideForAllTogglePressed UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed { userId : UserId, hidePoint : Coord AsciiUnit }
    | AnimationFrame Time.Posix


type ToBackend
    = RequestData (Bounds CellUnit)
    | GridChange (Nonempty Change.LocalChange)
    | ChangeViewBounds (Bounds CellUnit)


type BackendMsg
    = UserDisconnected SessionId ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (Nonempty Change)


type alias LoadingData_ =
    { user : UserId
    , grid : Grid
    , hiddenUsers : EverySet UserId
    , adminHiddenUsers : EverySet UserId
    , undoHistory : List (Dict RawCellCoord Int)
    , redoHistory : List (Dict RawCellCoord Int)
    , undoCurrent : Dict RawCellCoord Int
    , viewBounds : Bounds CellUnit
    }
