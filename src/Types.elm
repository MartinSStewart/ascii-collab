module Types exposing (BackendModel, BackendMsg(..), FrontendLoaded, FrontendModel(..), FrontendMsg(..), MouseState(..), ToBackend(..), ToFrontend(..))

import Ascii exposing (Ascii)
import Browser exposing (UrlRequest)
import Browser.Navigation
import Cursor exposing (Cursor)
import Dict exposing (Dict)
import Grid exposing (Grid, ServerGrid)
import GridCell exposing (Cell)
import Helper exposing (Coord)
import Keyboard
import Lamdera exposing (ClientId, SessionId)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Set exposing (Set)
import Time
import Units exposing (CellUnit, ScreenCoordinate, WorldCoordinate, WorldPixel)
import Url exposing (Url)
import User exposing (User, UserId)
import WebGL.Texture exposing (Texture)


type FrontendModel
    = Loading { key : Browser.Navigation.Key }
    | Loaded FrontendLoaded


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , grid : Grid
    , viewPoint : Point2d WorldPixel WorldCoordinate
    , cursor : Cursor
    , texture : Maybe Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Coord Pixels
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , mouseState : MouseState
    , userId : UserId
    }


type MouseState
    = MouseLeftUp
    | MouseLeftDown { start : Point2d Pixels ScreenCoordinate, current : Point2d Pixels ScreenCoordinate }


type alias BackendModel =
    { grid : ServerGrid, users : Set ( SessionId, ClientId ), changeCount : Int }


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
    | MouseDown (Point2d Pixels ScreenCoordinate)
    | MouseUp (Point2d Pixels ScreenCoordinate)
    | MouseMove (Point2d Pixels ScreenCoordinate)


type ToBackend
    = NoOpToBackend
    | RequestData
    | GridChange { changes : List Change }


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | LoadingData { userId : UserId, grid : ServerGrid }
    | GridChangeBroadcast { changes : List Change, changeId : Int, user : UserId }


type alias Change =
    { cellPosition : Coord CellUnit, localPosition : Int, change : List Ascii }
