module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , Change(..)
    , FrontendLoaded
    , FrontendLoading
    , FrontendModel(..)
    , FrontendMsg(..)
    , LoadingData_
    , LocalChange(..)
    , LocalGrid
    , MouseButtonState(..)
    , ServerChange(..)
    , ToBackend(..)
    , ToFrontend(..)
    , ToolType(..)
    )

import Browser exposing (UrlRequest)
import Browser.Navigation
import Cursor exposing (Cursor)
import Dict exposing (Dict)
import EverySet exposing (EverySet)
import Grid exposing (Grid, LocalChange)
import Helper exposing (Coord)
import Html.Events.Extra.Mouse exposing (Button)
import Keyboard
import Lamdera exposing (ClientId, SessionId)
import List.Nonempty exposing (Nonempty)
import LocalModel exposing (LocalModel)
import Math.Vector2 exposing (Vec2)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Set exposing (Set)
import Time
import Units exposing (CellUnit, ScreenCoordinate, WorldCoordinate, WorldPixel)
import Url exposing (Url)
import User exposing (RawUserId, UserData, UserId)
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
    }


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : LocalModel Change LocalGrid
    , meshes : Dict ( Int, Int ) (WebGL.Mesh Grid.Vertex)
    , cursorMesh : WebGL.Mesh { position : Vec2 }
    , viewPoint : Point2d WorldPixel WorldCoordinate
    , cursor : Cursor
    , texture : Maybe Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Coord Pixels
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , mouseMiddle : MouseButtonState
    , pendingChanges : List LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userPressHighlighted : Maybe UserId
    , userHoverHighlighted : Maybe UserId
    }


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange


type LocalChange
    = LocalGridChange Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalToggleUserVisibility UserId


type ServerChange
    = ServerGridChange Grid.Change
    | ServerUndoPoint { userId : UserId, undoPoints : Dict ( Int, Int ) Int }
    | ServerUserNew ( UserId, UserData )
    | ServerUserIsOnline UserId Bool


type alias LocalGrid =
    { grid : Grid
    , undoHistory : List (Dict ( Int, Int ) Int)
    , redoHistory : List (Dict ( Int, Int ) Int)
    , user : ( UserId, UserData )
    , otherUsers : List ( UserId, UserData )
    , hiddenUsers : EverySet UserId
    }


type ToolType
    = DragTool
    | SelectTool
    | HideUserTool


type MouseButtonState
    = MouseButtonUp
    | MouseButtonDown
        { start : Point2d Pixels ScreenCoordinate
        , start_ : Point2d WorldPixel WorldCoordinate
        , current : Point2d Pixels ScreenCoordinate
        }


type alias BackendModel =
    { grid : Grid
    , undoPoints :
        Dict RawUserId
            { undoHistory : List (Dict ( Int, Int ) Int)
            , redoHistory : List (Dict ( Int, Int ) Int)
            }
    , userSessions : Dict SessionId { clientIds : Set ClientId, userId : UserId }
    , users : Dict RawUserId { userData : UserData, hiddenUsers : EverySet UserId }
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
    | ToggleUserVisibilityPressed UserId
    | UserColorSquarePressed UserId
    | UserTagMouseEntered UserId
    | UserTagMouseExited UserId


type ToBackend
    = NoOpToBackend
    | RequestData
    | GridChange (Nonempty LocalChange)


type BackendMsg
    = NoOpBackendMsg
    | UserDisconnected SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | LoadingData LoadingData_
    | ServerChangeBroadcast (Nonempty ServerChange)
    | LocalChangeResponse (Nonempty LocalChange)


type alias LoadingData_ =
    { user : ( UserId, UserData )
    , grid : Grid
    , otherUsers : List ( UserId, UserData )
    , hiddenUsers : EverySet UserId
    , undoHistory : List (Dict ( Int, Int ) Int)
    , redoHistory : List (Dict ( Int, Int ) Int)
    }
