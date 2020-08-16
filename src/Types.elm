module Types exposing
    ( BackendModel
    , BackendMsg(..)
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

import Browser exposing (UrlRequest)
import Browser.Navigation
import Cursor exposing (Cursor)
import Dict exposing (Dict)
import Grid exposing (ChangeBroadcast, Grid)
import Helper exposing (Coord)
import Html.Events.Extra.Mouse exposing (Button)
import Keyboard
import Lamdera exposing (ClientId, SessionId)
import List.Nonempty exposing (Nonempty)
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
    , grid : Grid
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
    , userId : UserId
    , pendingChanges : List Grid.Change
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
    { grid : Grid, users : Set ( SessionId, ClientId ) }


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
    | GridChange { changes : Nonempty Grid.Change }


type BackendMsg
    = NoOpBackendMsg
    | UserDisconnected SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | LoadingData { userId : UserId, grid : Grid }
    | GridChangeBroadcast { changes : Nonempty ChangeBroadcast, user : UserId }
