module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V1.Cursor
import Dict
import Evergreen.V1.Grid
import Evergreen.V1.Helper
import Keyboard
import Lamdera
import List.Nonempty
import Math.Vector2
import Pixels
import Evergreen.V1.Point2d
import Quantity
import Set
import Time
import Evergreen.V1.Units
import Url
import Evergreen.V1.User
import WebGL
import WebGL.Texture


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    }


type MouseState
    = MouseLeftUp
    | MouseLeftDown 
    { start : (Evergreen.V1.Point2d.Point2d Pixels.Pixels Evergreen.V1.Units.ScreenCoordinate)
    , current : (Evergreen.V1.Point2d.Point2d Pixels.Pixels Evergreen.V1.Units.ScreenCoordinate)
    }


type alias FrontendLoaded = 
    { key : Browser.Navigation.Key
    , grid : Evergreen.V1.Grid.Grid
    , meshes : (Dict.Dict (Int, Int) (WebGL.Mesh Vertex))
    , viewPoint : (Evergreen.V1.Point2d.Point2d Evergreen.V1.Units.WorldPixel Evergreen.V1.Units.WorldCoordinate)
    , cursor : Evergreen.V1.Cursor.Cursor
    , texture : (Maybe WebGL.Texture.Texture)
    , pressedKeys : (List Keyboard.Key)
    , windowSize : (Evergreen.V1.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V1.Units.WorldPixel Pixels.Pixels))
    , mouseState : MouseState
    , userId : Evergreen.V1.User.UserId
    }


type FrontendModel
    = Loading 
    { key : Browser.Navigation.Key
    }
    | Loaded FrontendLoaded


type alias BackendModel =
    { grid : Evergreen.V1.Grid.Grid
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
    | WindowResized (Evergreen.V1.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V1.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown (Evergreen.V1.Point2d.Point2d Pixels.Pixels Evergreen.V1.Units.ScreenCoordinate)
    | MouseUp (Evergreen.V1.Point2d.Point2d Pixels.Pixels Evergreen.V1.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V1.Point2d.Point2d Pixels.Pixels Evergreen.V1.Units.ScreenCoordinate)


type ToBackend
    = NoOpToBackend
    | RequestData
    | GridChange 
    { changes : (List.Nonempty.Nonempty Evergreen.V1.Grid.Change)
    }


type BackendMsg
    = NoOpBackendMsg
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | LoadingData 
    { userId : Evergreen.V1.User.UserId
    , grid : Evergreen.V1.Grid.Grid
    }
    | GridChangeBroadcast 
    { changes : (List.Nonempty.Nonempty Evergreen.V1.Grid.ChangeBroadcast)
    , user : Evergreen.V1.User.UserId
    }