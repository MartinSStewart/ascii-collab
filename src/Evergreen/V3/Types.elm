module Evergreen.V3.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V3.Cursor
import Dict
import Evergreen.V3.Grid
import Evergreen.V3.Helper
import Keyboard
import Lamdera
import List.Nonempty
import Math.Vector2
import Pixels
import Evergreen.V3.Point2d
import Quantity
import Set
import Time
import Evergreen.V3.Units
import Url
import Evergreen.V3.User
import WebGL
import WebGL.Texture


type alias FrontendLoading = 
    { key : Browser.Navigation.Key
    , windowSize : (Evergreen.V3.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V3.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    }


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    }


type MouseState
    = MouseLeftUp
    | MouseLeftDown 
    { start : (Evergreen.V3.Point2d.Point2d Pixels.Pixels Evergreen.V3.Units.ScreenCoordinate)
    , current : (Evergreen.V3.Point2d.Point2d Pixels.Pixels Evergreen.V3.Units.ScreenCoordinate)
    }


type alias FrontendLoaded = 
    { key : Browser.Navigation.Key
    , grid : Evergreen.V3.Grid.Grid
    , meshes : (Dict.Dict (Int, Int) (WebGL.Mesh Vertex))
    , viewPoint : (Evergreen.V3.Point2d.Point2d Evergreen.V3.Units.WorldPixel Evergreen.V3.Units.WorldCoordinate)
    , cursor : Evergreen.V3.Cursor.Cursor
    , texture : (Maybe WebGL.Texture.Texture)
    , pressedKeys : (List Keyboard.Key)
    , windowSize : (Evergreen.V3.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V3.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , mouseState : MouseState
    , userId : Evergreen.V3.User.UserId
    , pendingChanges : (List Evergreen.V3.Grid.Change)
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendModel =
    { grid : Evergreen.V3.Grid.Grid
    , users : (Set.Set (Lamdera.SessionId, Lamdera.ClientId))
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | Step Time.Posix
    | WindowResized (Evergreen.V3.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V3.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown (Evergreen.V3.Point2d.Point2d Pixels.Pixels Evergreen.V3.Units.ScreenCoordinate)
    | MouseUp (Evergreen.V3.Point2d.Point2d Pixels.Pixels Evergreen.V3.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V3.Point2d.Point2d Pixels.Pixels Evergreen.V3.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int


type ToBackend
    = NoOpToBackend
    | RequestData
    | GridChange 
    { changes : (List.Nonempty.Nonempty Evergreen.V3.Grid.Change)
    }


type BackendMsg
    = NoOpBackendMsg
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | LoadingData 
    { userId : Evergreen.V3.User.UserId
    , grid : Evergreen.V3.Grid.Grid
    }
    | GridChangeBroadcast 
    { changes : (List.Nonempty.Nonempty Evergreen.V3.Grid.ChangeBroadcast)
    , user : Evergreen.V3.User.UserId
    }