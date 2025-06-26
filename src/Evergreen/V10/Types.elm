module Evergreen.V10.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V10.Bounds
import Evergreen.V10.Change
import Evergreen.V10.Cursor
import Evergreen.V10.Grid
import Evergreen.V10.Helper
import Evergreen.V10.LocalGrid
import Evergreen.V10.LocalModel
import Evergreen.V10.Point2d
import Evergreen.V10.Units
import Evergreen.V10.User
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
    , windowSize : Evergreen.V10.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V10.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V10.Helper.Coord Evergreen.V10.Units.AsciiUnit
    }


type MouseButtonState
    = MouseButtonUp
    | MouseButtonDown
        { start : Evergreen.V10.Point2d.Point2d Pixels.Pixels Evergreen.V10.Units.ScreenCoordinate
        , start_ : Evergreen.V10.Point2d.Point2d Evergreen.V10.Units.WorldPixel Evergreen.V10.Units.WorldCoordinate
        , current : Evergreen.V10.Point2d.Point2d Pixels.Pixels Evergreen.V10.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HideUserTool (Maybe Evergreen.V10.User.UserId)


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V10.LocalModel.LocalModel Evergreen.V10.Change.Change Evergreen.V10.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V10.Helper.RawCellCoord (WebGL.Mesh Evergreen.V10.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V10.Point2d.Point2d Evergreen.V10.Units.WorldPixel Evergreen.V10.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V10.Point2d.Point2d Evergreen.V10.Units.WorldPixel Evergreen.V10.Units.WorldCoordinate
    , cursor : Evergreen.V10.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V10.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V10.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V10.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userPressHighlighted : Maybe Evergreen.V10.User.UserId
    , userHoverHighlighted : Maybe Evergreen.V10.User.UserId
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
    { userData : Evergreen.V10.User.UserData
    , hiddenUsers : SeqSet.SeqSet Evergreen.V10.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V10.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V10.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V10.Helper.RawCellCoord Int
    }


type alias BackendModel =
    { grid : Evergreen.V10.Grid.Grid
    , userSessions :
        Dict.Dict
            SessionId
            { clientIds : Dict.Dict ClientId (Evergreen.V10.Bounds.Bounds Evergreen.V10.Units.CellUnit)
            , userId : Evergreen.V10.User.UserId
            }
    , users : Dict.Dict Evergreen.V10.User.RawUserId BackendUserData
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V10.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V10.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V10.Point2d.Point2d Pixels.Pixels Evergreen.V10.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V10.Point2d.Point2d Pixels.Pixels Evergreen.V10.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V10.Point2d.Point2d Pixels.Pixels Evergreen.V10.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V10.Point2d.Point2d Pixels.Pixels Evergreen.V10.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V10.User.UserId
    | UserColorSquarePressed Evergreen.V10.User.UserId
    | UserTagMouseEntered Evergreen.V10.User.UserId
    | UserTagMouseExited Evergreen.V10.User.UserId
    | HideForAllTogglePressed Evergreen.V10.User.UserId
    | ToggleAdminEnabledPressed


type ToBackend
    = RequestData (Evergreen.V10.Bounds.Bounds Evergreen.V10.Units.CellUnit)
    | GridChange (List.Nonempty.Nonempty Evergreen.V10.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V10.Bounds.Bounds Evergreen.V10.Units.CellUnit)


type BackendMsg
    = UserDisconnected SessionId ClientId


type alias LoadingData_ =
    { user : ( Evergreen.V10.User.UserId, Evergreen.V10.User.UserData )
    , grid : Evergreen.V10.Grid.Grid
    , otherUsers : List ( Evergreen.V10.User.UserId, Evergreen.V10.User.UserData )
    , hiddenUsers : SeqSet.SeqSet Evergreen.V10.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V10.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V10.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V10.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V10.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V10.Bounds.Bounds Evergreen.V10.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V10.Change.Change)
