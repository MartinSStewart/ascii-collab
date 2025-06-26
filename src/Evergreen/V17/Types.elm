module Evergreen.V17.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V17.Bounds
import Evergreen.V17.Change
import Evergreen.V17.Cursor
import Evergreen.V17.Grid
import Evergreen.V17.Helper
import Evergreen.V17.LocalGrid
import Evergreen.V17.LocalModel
import Evergreen.V17.Point2d
import Evergreen.V17.Units
import Evergreen.V17.User
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
    , windowSize : Evergreen.V17.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V17.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V17.Helper.Coord Evergreen.V17.Units.AsciiUnit
    }


type MouseButtonState
    = MouseButtonUp
    | MouseButtonDown
        { start : Evergreen.V17.Point2d.Point2d Pixels.Pixels Evergreen.V17.Units.ScreenCoordinate
        , start_ : Evergreen.V17.Point2d.Point2d Evergreen.V17.Units.WorldPixel Evergreen.V17.Units.WorldCoordinate
        , current : Evergreen.V17.Point2d.Point2d Pixels.Pixels Evergreen.V17.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe ( Evergreen.V17.User.UserId, Evergreen.V17.Helper.Coord Evergreen.V17.Units.AsciiUnit ))


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V17.LocalModel.LocalModel Evergreen.V17.Change.Change Evergreen.V17.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V17.Helper.RawCellCoord (WebGL.Mesh Evergreen.V17.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V17.Point2d.Point2d Evergreen.V17.Units.WorldPixel Evergreen.V17.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V17.Point2d.Point2d Evergreen.V17.Units.WorldPixel Evergreen.V17.Units.WorldCoordinate
    , cursor : Evergreen.V17.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V17.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V17.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V17.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userHoverHighlighted : Maybe Evergreen.V17.User.UserId
    , highlightContextMenu :
        Maybe
            { userId : Evergreen.V17.User.UserId
            , hidePoint : Evergreen.V17.Helper.Coord Evergreen.V17.Units.AsciiUnit
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
    { hiddenUsers : SeqSet.SeqSet Evergreen.V17.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V17.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V17.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V17.Helper.RawCellCoord Int
    }


type alias BackendModel =
    { grid : Evergreen.V17.Grid.Grid
    , userSessions :
        Dict.Dict
            SessionId
            { clientIds : Dict.Dict ClientId (Evergreen.V17.Bounds.Bounds Evergreen.V17.Units.CellUnit)
            , userId : Evergreen.V17.User.UserId
            }
    , users : Dict.Dict Evergreen.V17.User.RawUserId BackendUserData
    , usersHiddenRecently :
        List
            { reporter : Evergreen.V17.User.UserId
            , hiddenUser : Evergreen.V17.User.UserId
            , hidePoint : Evergreen.V17.Helper.Coord Evergreen.V17.Units.AsciiUnit
            }
    , userChangesRecently : Dict.Dict ( Evergreen.V17.User.RawUserId, Evergreen.V17.Helper.RawCellCoord ) Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V17.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V17.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V17.Point2d.Point2d Pixels.Pixels Evergreen.V17.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V17.Point2d.Point2d Pixels.Pixels Evergreen.V17.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V17.Point2d.Point2d Pixels.Pixels Evergreen.V17.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V17.Point2d.Point2d Pixels.Pixels Evergreen.V17.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V17.User.UserId
    | UserTagMouseEntered Evergreen.V17.User.UserId
    | UserTagMouseExited Evergreen.V17.User.UserId
    | HideForAllTogglePressed Evergreen.V17.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed
        { userId : Evergreen.V17.User.UserId
        , hidePoint : Evergreen.V17.Helper.Coord Evergreen.V17.Units.AsciiUnit
        }


type ToBackend
    = RequestData (Evergreen.V17.Bounds.Bounds Evergreen.V17.Units.CellUnit)
    | GridChange (List.Nonempty.Nonempty Evergreen.V17.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V17.Bounds.Bounds Evergreen.V17.Units.CellUnit)


type BackendMsg
    = UserDisconnected SessionId ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent


type alias LoadingData_ =
    { user : Evergreen.V17.User.UserId
    , grid : Evergreen.V17.Grid.Grid
    , hiddenUsers : SeqSet.SeqSet Evergreen.V17.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V17.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V17.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V17.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V17.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V17.Bounds.Bounds Evergreen.V17.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V17.Change.Change)
