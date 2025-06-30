module Evergreen.V24.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V24.Bounds
import Evergreen.V24.Change
import Evergreen.V24.Cursor
import Evergreen.V24.Grid
import Evergreen.V24.Helper
import Evergreen.V24.LocalGrid
import Evergreen.V24.LocalModel
import Evergreen.V24.Point2d
import Evergreen.V24.Units
import Evergreen.V24.User
import Html.Events.Extra.Mouse
import Keyboard
import List.Nonempty
import Math.Vector2
import Pixels
import Quantity
import SeqSet
import Time
import Url
import WebGL
import WebGL.Texture


type alias FrontendLoading =
    { key : Browser.Navigation.Key
    , windowSize : Evergreen.V24.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V24.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V24.Helper.Coord Evergreen.V24.Units.AsciiUnit
    }


type MouseButtonState
    = MouseButtonUp
    | MouseButtonDown
        { start : Evergreen.V24.Point2d.Point2d Pixels.Pixels Evergreen.V24.Units.ScreenCoordinate
        , start_ : Evergreen.V24.Point2d.Point2d Evergreen.V24.Units.WorldPixel Evergreen.V24.Units.WorldCoordinate
        , current : Evergreen.V24.Point2d.Point2d Pixels.Pixels Evergreen.V24.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe ( Evergreen.V24.User.UserId, Evergreen.V24.Helper.Coord Evergreen.V24.Units.AsciiUnit ))


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V24.LocalModel.LocalModel Evergreen.V24.Change.Change Evergreen.V24.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V24.Helper.RawCellCoord (WebGL.Mesh Evergreen.V24.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V24.Point2d.Point2d Evergreen.V24.Units.WorldPixel Evergreen.V24.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V24.Point2d.Point2d Evergreen.V24.Units.WorldPixel Evergreen.V24.Units.WorldCoordinate
    , cursor : Evergreen.V24.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V24.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V24.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V24.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userHoverHighlighted : Maybe Evergreen.V24.User.UserId
    , highlightContextMenu :
        Maybe
            { userId : Evergreen.V24.User.UserId
            , hidePoint : Evergreen.V24.Helper.Coord Evergreen.V24.Units.AsciiUnit
            }
    , adminEnabled : Bool
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias SessionId =
    String


type alias ClientId =
    String


type alias BackendUserData =
    { hiddenUsers : SeqSet.SeqSet Evergreen.V24.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V24.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V24.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V24.Helper.RawCellCoord Int
    }


type alias BackendModel =
    { grid : Evergreen.V24.Grid.Grid
    , userSessions :
        Dict.Dict
            SessionId
            { clientIds : Dict.Dict ClientId (Evergreen.V24.Bounds.Bounds Evergreen.V24.Units.CellUnit)
            , userId : Evergreen.V24.User.UserId
            }
    , users : Dict.Dict Evergreen.V24.User.RawUserId BackendUserData
    , usersHiddenRecently :
        List
            { reporter : Evergreen.V24.User.UserId
            , hiddenUser : Evergreen.V24.User.UserId
            , hidePoint : Evergreen.V24.Helper.Coord Evergreen.V24.Units.AsciiUnit
            }
    , userChangesRecently : Dict.Dict ( Evergreen.V24.User.RawUserId, Evergreen.V24.Helper.RawCellCoord ) Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V24.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V24.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V24.Point2d.Point2d Pixels.Pixels Evergreen.V24.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V24.Point2d.Point2d Pixels.Pixels Evergreen.V24.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V24.Point2d.Point2d Pixels.Pixels Evergreen.V24.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V24.Point2d.Point2d Pixels.Pixels Evergreen.V24.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V24.User.UserId
    | UserTagMouseEntered Evergreen.V24.User.UserId
    | UserTagMouseExited Evergreen.V24.User.UserId
    | HideForAllTogglePressed Evergreen.V24.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed
        { userId : Evergreen.V24.User.UserId
        , hidePoint : Evergreen.V24.Helper.Coord Evergreen.V24.Units.AsciiUnit
        }
    | AnimationFrame Time.Posix


type ToBackend
    = RequestData (Evergreen.V24.Bounds.Bounds Evergreen.V24.Units.CellUnit)
    | GridChange (List.Nonempty.Nonempty Evergreen.V24.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V24.Bounds.Bounds Evergreen.V24.Units.CellUnit)


type BackendMsg
    = UserDisconnected SessionId ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent


type alias LoadingData_ =
    { user : Evergreen.V24.User.UserId
    , grid : Evergreen.V24.Grid.Grid
    , hiddenUsers : SeqSet.SeqSet Evergreen.V24.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V24.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V24.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V24.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V24.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V24.Bounds.Bounds Evergreen.V24.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V24.Change.Change)
