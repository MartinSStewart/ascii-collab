module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , BackendUserData
    , ClientId
    , FrontendLoaded
    , FrontendLoading
    , FrontendModel(..)
    , FrontendMsg(..)
    , LoadingData_
    , MouseButtonState(..)
    , SessionId
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
import EverySet exposing (EverySet)
import Grid exposing (Grid)
import Helper exposing (Coord, RawCellCoord)
import Html.Events.Extra.Mouse exposing (Button)
import Keyboard
import List.Nonempty exposing (Nonempty)
import LocalGrid exposing (LocalGrid)
import LocalModel exposing (LocalModel)
import Math.Vector2 exposing (Vec2)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import SendGrid
import Time
import Units exposing (AsciiUnit, CellUnit, ScreenCoordinate, WorldCoordinate, WorldPixel)
import Url exposing (Url)
import User exposing (RawUserId, UserData, UserId)
import WebGL
import WebGL.Texture exposing (Texture)


type alias SessionId =
    String


type alias ClientId =
    String


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
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userPressHighlighted : Maybe UserId
    , userHoverHighlighted : Maybe UserId
    , adminEnabled : Bool
    }


type ToolType
    = DragTool
    | SelectTool
    | HideUserTool (Maybe ( UserId, Coord AsciiUnit ))


type MouseButtonState
    = MouseButtonUp
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
    { userData : UserData
    , hiddenUsers : EverySet UserId
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
    | UserColorSquarePressed UserId
    | UserTagMouseEntered UserId
    | UserTagMouseExited UserId
    | HideForAllTogglePressed UserId
    | ToggleAdminEnabledPressed


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
    { user : ( UserId, UserData )
    , grid : Grid
    , otherUsers : List ( UserId, UserData )
    , hiddenUsers : EverySet UserId
    , adminHiddenUsers : EverySet UserId
    , undoHistory : List (Dict RawCellCoord Int)
    , redoHistory : List (Dict RawCellCoord Int)
    , undoCurrent : Dict RawCellCoord Int
    , viewBounds : Bounds CellUnit
    }
