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
    , Vertex
    )

import Browser exposing (UrlRequest)
import Browser.Navigation
import Cursor exposing (Cursor)
import Dict exposing (Dict)
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
import User exposing (RawUserId, User, UserId)
import WebGL
import WebGL.Texture exposing (Texture)


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias Vertex =
    { position : Vec2, texturePosition : Vec2 }


type alias FrontendLoading =
    { key : Browser.Navigation.Key
    , windowSize : Coord Pixels
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , zoomFactor : Int
    }


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : LocalModel Change LocalGrid
    , meshes : Dict ( Int, Int ) (WebGL.Mesh Vertex)
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
    , user : User
    , otherUsers : List User
    , pendingChanges : List LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    }


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange


type LocalChange
    = LocalGridChange Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | AddUndo


type ServerChange
    = ServerGridChange Grid.Change
    | ServerUndoPoint { userId : UserId, undoPoints : Dict ( Int, Int ) Int }


type alias LocalGrid =
    { grid : Grid
    , undoHistory : List (Dict ( Int, Int ) Int)
    , redoHistory : List (Dict ( Int, Int ) Int)
    }


type ToolType
    = DragTool
    | RectangleTool
    | SelectTool


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
    , userSessions : Set ( SessionId, ClientId )
    , users : Dict SessionId User
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | Step Time.Posix
    | WindowResized (Coord Pixels)
    | GotDevicePixelRatio (Quantity Float (Rate WorldPixel Pixels))
    | UserTyped String
    | MouseDown Button (Point2d Pixels ScreenCoordinate)
    | MouseUp Button (Point2d Pixels ScreenCoordinate)
    | MouseMove (Point2d Pixels ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed


type ToBackend
    = NoOpToBackend
    | RequestData
    | GridChange (Nonempty LocalChange)
    | UserRename String


type BackendMsg
    = NoOpBackendMsg
    | UserDisconnected SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | LoadingData LoadingData_
    | ServerChangeBroadcast (Nonempty ServerChange)
    | LocalChangeResponse (Nonempty LocalChange)
    | NewUserBroadcast User
    | UserModifiedBroadcast User


type alias LoadingData_ =
    { user : User
    , grid : Grid
    , otherUsers : List User
    , undoHistory : List (Dict ( Int, Int ) Int)
    , redoHistory : List (Dict ( Int, Int ) Int)
    }
