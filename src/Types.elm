module Types exposing (BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), ToBackend(..), ToFrontend(..))

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Grid exposing (Grid)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Url exposing (Url)
import WebGL.Texture exposing (Texture)


type WorldCoordinate
    = WorldCoordinate Never


type alias FrontendModel =
    { key : Key
    , grid : Grid
    , viewPoint : Point2d Grid.AsciiUnit WorldCoordinate
    , cursorPoint : Maybe ( Quantity Int Grid.AsciiUnit, Quantity Int Grid.AsciiUnit )
    , texture : Maybe Texture
    }


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error Texture)


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
