module Evergreen.V38.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Duration
import Evergreen.V38.Bounds
import Evergreen.V38.Change
import Evergreen.V38.Cursor
import Evergreen.V38.Grid
import Evergreen.V38.Helper
import Evergreen.V38.LocalGrid
import Evergreen.V38.LocalModel
import Evergreen.V38.Point2d
import Evergreen.V38.Units
import Evergreen.V38.User
import Html.Events.Extra.Mouse
import Keyboard
import Lamdera
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
    , windowSize : Evergreen.V38.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V38.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V38.Helper.Coord Evergreen.V38.Units.AsciiUnit
    , mousePosition : Evergreen.V38.Point2d.Point2d Pixels.Pixels Evergreen.V38.Units.ScreenCoordinate
    }


type MouseButtonState
    = MouseButtonUp
        { current : Evergreen.V38.Point2d.Point2d Pixels.Pixels Evergreen.V38.Units.ScreenCoordinate
        }
    | MouseButtonDown
        { start : Evergreen.V38.Point2d.Point2d Pixels.Pixels Evergreen.V38.Units.ScreenCoordinate
        , start_ : Evergreen.V38.Point2d.Point2d Evergreen.V38.Units.WorldPixel Evergreen.V38.Units.WorldCoordinate
        , current : Evergreen.V38.Point2d.Point2d Pixels.Pixels Evergreen.V38.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe ( Evergreen.V38.User.UserId, Evergreen.V38.Helper.Coord Evergreen.V38.Units.AsciiUnit ))


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V38.LocalModel.LocalModel Evergreen.V38.Change.Change Evergreen.V38.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V38.Helper.RawCellCoord (WebGL.Mesh Evergreen.V38.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V38.Point2d.Point2d Evergreen.V38.Units.WorldPixel Evergreen.V38.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V38.Point2d.Point2d Evergreen.V38.Units.WorldPixel Evergreen.V38.Units.WorldCoordinate
    , cursor : Evergreen.V38.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V38.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V38.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : Maybe ( Time.Posix, Evergreen.V38.Point2d.Point2d Pixels.Pixels Evergreen.V38.Units.ScreenCoordinate )
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V38.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userHoverHighlighted : Maybe Evergreen.V38.User.UserId
    , highlightContextMenu :
        Maybe
            { userId : Evergreen.V38.User.UserId
            , hidePoint : Evergreen.V38.Helper.Coord Evergreen.V38.Units.AsciiUnit
            }
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData =
    { hiddenUsers : SeqSet.SeqSet Evergreen.V38.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V38.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V38.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V38.Helper.RawCellCoord Int
    }


type alias BackendModel =
    { grid : Evergreen.V38.Grid.Grid
    , userSessions :
        Dict.Dict
            Lamdera.SessionId
            { clientIds : Dict.Dict Lamdera.ClientId (Evergreen.V38.Bounds.Bounds Evergreen.V38.Units.CellUnit)
            , userId : Evergreen.V38.User.UserId
            }
    , users : Dict.Dict Evergreen.V38.User.RawUserId BackendUserData
    , usersHiddenRecently :
        List
            { reporter : Evergreen.V38.User.UserId
            , hiddenUser : Evergreen.V38.User.UserId
            , hidePoint : Evergreen.V38.Helper.Coord Evergreen.V38.Units.AsciiUnit
            }
    , userChangesRecently : Dict.Dict ( Evergreen.V38.User.RawUserId, Evergreen.V38.Helper.RawCellCoord ) Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V38.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V38.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V38.Point2d.Point2d Pixels.Pixels Evergreen.V38.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V38.Point2d.Point2d Pixels.Pixels Evergreen.V38.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V38.Point2d.Point2d Pixels.Pixels Evergreen.V38.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V38.Point2d.Point2d Pixels.Pixels Evergreen.V38.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V38.User.UserId
    | UserTagMouseEntered Evergreen.V38.User.UserId
    | UserTagMouseExited Evergreen.V38.User.UserId
    | HideForAllTogglePressed Evergreen.V38.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed
        { userId : Evergreen.V38.User.UserId
        , hidePoint : Evergreen.V38.Helper.Coord Evergreen.V38.Units.AsciiUnit
        }
    | AnimationFrame Time.Posix


type ToBackend
    = RequestData (Evergreen.V38.Bounds.Bounds Evergreen.V38.Units.CellUnit)
    | GridChange (List.Nonempty.Nonempty Evergreen.V38.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V38.Bounds.Bounds Evergreen.V38.Units.CellUnit)


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent


type alias LoadingData_ =
    { user : Evergreen.V38.User.UserId
    , grid : Evergreen.V38.Grid.Grid
    , hiddenUsers : SeqSet.SeqSet Evergreen.V38.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V38.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V38.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V38.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V38.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V38.Bounds.Bounds Evergreen.V38.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V38.Change.Change)
