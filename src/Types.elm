module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , Change_
    , FrontendLoaded
    , FrontendLoading
    , FrontendModel(..)
    , FrontendMsg(..)
    , MouseButtonState(..)
    , ToBackend(..)
    , ToFrontend(..)
    , ToolType(..)
    , Vertex
    )

import Ascii exposing (Ascii)
import Browser exposing (UrlRequest)
import Browser.Navigation
import Cursor exposing (Cursor)
import Dict exposing (Dict)
import Grid exposing (Grid)
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
import User exposing (User, UserId)
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
    , localModel : LocalModel Grid.Change Grid
    , meshes : Dict ( Int, Int ) (WebGL.Mesh Vertex)
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
    , pendingChanges : List Change_
    , tool : ToolType
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
    { grid : Grid, userSessions : Set ( SessionId, ClientId ), users : Dict SessionId User }


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


type ToBackend
    = NoOpToBackend
    | RequestData
    | GridChange (Nonempty Change_)
    | UserRename String


type alias Change_ =
    { cellPosition : Coord Units.CellUnit, localPosition : Int, change : Nonempty Ascii }


type BackendMsg
    = NoOpBackendMsg
    | UserDisconnected SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | LoadingData { user : User, grid : Grid, otherUsers : List User }
    | GridChangeBroadcast (Nonempty Grid.Change)
    | NewUserBroadcast User
    | UserModifiedBroadcast User
