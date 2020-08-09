module Types exposing (BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), ToBackend(..), ToFrontend(..))

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Cursor exposing (Cursor)
import Grid exposing (Grid)
import Keyboard
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Time
import Units
import Url exposing (Url)
import WebGL.Texture exposing (Texture)


type WorldCoordinate
    = WorldCoordinate Never


type alias FrontendModel =
    { key : Key
    , grid : Grid
    , viewPoint : ( Quantity Int Units.WorldPixel, Quantity Int Units.WorldPixel )
    , cursor : Maybe Cursor
    , texture : Maybe Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : ( Quantity Int Pixels, Quantity Int Pixels )
    , devicePixelRatio : Float
    }


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
    | WindowResized ( Quantity Int Pixels, Quantity Int Pixels )
    | GotDevicePixelRatio Float
    | UserTyped String
    | MouseDown ( Quantity Int Pixels, Quantity Int Pixels )


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
