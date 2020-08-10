module Types exposing (BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), MouseState(..), ToBackend(..), ToFrontend(..))

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Cursor exposing (Cursor)
import Grid exposing (Grid)
import Helper exposing (Coord)
import Keyboard
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Time
import Units exposing (ScreenCoordinate, WorldCoordinate, WorldPixel)
import Url exposing (Url)
import WebGL.Texture exposing (Texture)


type alias FrontendModel =
    { key : Key
    , grid : Grid
    , viewPoint : Point2d WorldPixel WorldCoordinate
    , cursor : Maybe Cursor
    , texture : Maybe Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Coord Pixels
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , mouseState : MouseState
    }


type MouseState
    = MouseLeftUp
    | MouseLeftDown { start : Point2d Pixels ScreenCoordinate, current : Point2d Pixels ScreenCoordinate }


type alias BackendModel =
    {}


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


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
