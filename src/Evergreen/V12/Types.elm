module Evergreen.V12.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V12.Bounds
import Evergreen.V12.Change
import Evergreen.V12.Cursor
import Evergreen.V12.Grid
import Evergreen.V12.Helper
import Evergreen.V12.LocalGrid
import Evergreen.V12.LocalModel
import Evergreen.V12.Point2d
import Evergreen.V12.Units
import Evergreen.V12.User
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
    , windowSize : Evergreen.V12.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V12.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V12.Helper.Coord Evergreen.V12.Units.AsciiUnit
    }


type MouseButtonState
    = MouseButtonUp
    | MouseButtonDown
        { start : Evergreen.V12.Point2d.Point2d Pixels.Pixels Evergreen.V12.Units.ScreenCoordinate
        , start_ : Evergreen.V12.Point2d.Point2d Evergreen.V12.Units.WorldPixel Evergreen.V12.Units.WorldCoordinate
        , current : Evergreen.V12.Point2d.Point2d Pixels.Pixels Evergreen.V12.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HideUserTool (Maybe Evergreen.V12.User.UserId)


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V12.LocalModel.LocalModel Evergreen.V12.Change.Change Evergreen.V12.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V12.Helper.RawCellCoord (WebGL.Mesh Evergreen.V12.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V12.Point2d.Point2d Evergreen.V12.Units.WorldPixel Evergreen.V12.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V12.Point2d.Point2d Evergreen.V12.Units.WorldPixel Evergreen.V12.Units.WorldCoordinate
    , cursor : Evergreen.V12.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V12.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V12.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V12.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userPressHighlighted : Maybe Evergreen.V12.User.UserId
    , userHoverHighlighted : Maybe Evergreen.V12.User.UserId
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
    { userData : Evergreen.V12.User.UserData
    , hiddenUsers : SeqSet.SeqSet Evergreen.V12.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V12.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V12.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V12.Helper.RawCellCoord Int
    }


type alias BackendModel =
    { grid : Evergreen.V12.Grid.Grid
    , userSessions :
        Dict.Dict
            SessionId
            { clientIds : Dict.Dict ClientId (Evergreen.V12.Bounds.Bounds Evergreen.V12.Units.CellUnit)
            , userId : Evergreen.V12.User.UserId
            }
    , users : Dict.Dict Evergreen.V12.User.RawUserId BackendUserData
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V12.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V12.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V12.Point2d.Point2d Pixels.Pixels Evergreen.V12.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V12.Point2d.Point2d Pixels.Pixels Evergreen.V12.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V12.Point2d.Point2d Pixels.Pixels Evergreen.V12.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V12.Point2d.Point2d Pixels.Pixels Evergreen.V12.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V12.User.UserId
    | UserColorSquarePressed Evergreen.V12.User.UserId
    | UserTagMouseEntered Evergreen.V12.User.UserId
    | UserTagMouseExited Evergreen.V12.User.UserId
    | HideForAllTogglePressed Evergreen.V12.User.UserId
    | ToggleAdminEnabledPressed


type ToBackend
    = RequestData (Evergreen.V12.Bounds.Bounds Evergreen.V12.Units.CellUnit)
    | GridChange (List.Nonempty.Nonempty Evergreen.V12.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V12.Bounds.Bounds Evergreen.V12.Units.CellUnit)


type BackendMsg
    = UserDisconnected SessionId ClientId


type alias LoadingData_ =
    { user : ( Evergreen.V12.User.UserId, Evergreen.V12.User.UserData )
    , grid : Evergreen.V12.Grid.Grid
    , otherUsers : List ( Evergreen.V12.User.UserId, Evergreen.V12.User.UserData )
    , hiddenUsers : SeqSet.SeqSet Evergreen.V12.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V12.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V12.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V12.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V12.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V12.Bounds.Bounds Evergreen.V12.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V12.Change.Change)
