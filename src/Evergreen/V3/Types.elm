module Evergreen.V3.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V3.Cursor
import Dict
import Evergreen.V3.Grid
import Evergreen.V3.Helper
import Html.Events.Extra.Mouse
import Keyboard
import Lamdera
import List.Nonempty
import Evergreen.V3.LocalModel
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


type LocalChange
    = LocalGridChange Evergreen.V3.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | AddUndo


type ServerChange
    = ServerGridChange Evergreen.V3.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V3.User.UserId
    , undoPoints : (Dict.Dict (Int, Int) Int)
    }


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange


type alias LocalGrid = 
    { grid : Evergreen.V3.Grid.Grid
    , undoHistory : (List (Dict.Dict (Int, Int) Int))
    , redoHistory : (List (Dict.Dict (Int, Int) Int))
    }


type alias Vertex = 
    { position : Math.Vector2.Vec2
    , texturePosition : Math.Vector2.Vec2
    }


type MouseButtonState
    = MouseButtonUp
    | MouseButtonDown 
    { start : (Evergreen.V3.Point2d.Point2d Pixels.Pixels Evergreen.V3.Units.ScreenCoordinate)
    , start_ : (Evergreen.V3.Point2d.Point2d Evergreen.V3.Units.WorldPixel Evergreen.V3.Units.WorldCoordinate)
    , current : (Evergreen.V3.Point2d.Point2d Pixels.Pixels Evergreen.V3.Units.ScreenCoordinate)
    }


type ToolType
    = DragTool
    | RectangleTool
    | SelectTool


type alias FrontendLoaded = 
    { key : Browser.Navigation.Key
    , localModel : (Evergreen.V3.LocalModel.LocalModel Change LocalGrid)
    , meshes : (Dict.Dict (Int, Int) (WebGL.Mesh Vertex))
    , cursorMesh : (WebGL.Mesh 
    { position : Math.Vector2.Vec2
    })
    , viewPoint : (Evergreen.V3.Point2d.Point2d Evergreen.V3.Units.WorldPixel Evergreen.V3.Units.WorldCoordinate)
    , cursor : Evergreen.V3.Cursor.Cursor
    , texture : (Maybe WebGL.Texture.Texture)
    , pressedKeys : (List Keyboard.Key)
    , windowSize : (Evergreen.V3.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V3.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , mouseMiddle : MouseButtonState
    , user : Evergreen.V3.User.User
    , otherUsers : (List Evergreen.V3.User.User)
    , pendingChanges : (List LocalChange)
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : (Maybe Time.Posix)
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendModel =
    { grid : Evergreen.V3.Grid.Grid
    , undoPoints : (Dict.Dict Evergreen.V3.User.RawUserId 
    { undoHistory : (List (Dict.Dict (Int, Int) Int))
    , redoHistory : (List (Dict.Dict (Int, Int) Int))
    })
    , userSessions : (Set.Set (Lamdera.SessionId, Lamdera.ClientId))
    , users : (Dict.Dict Lamdera.SessionId Evergreen.V3.User.User)
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
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V3.Point2d.Point2d Pixels.Pixels Evergreen.V3.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V3.Point2d.Point2d Pixels.Pixels Evergreen.V3.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V3.Point2d.Point2d Pixels.Pixels Evergreen.V3.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V3.Point2d.Point2d Pixels.Pixels Evergreen.V3.Units.ScreenCoordinate)
    | TouchMoveElapsed (Evergreen.V3.Point2d.Point2d Pixels.Pixels Evergreen.V3.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed


type ToBackend
    = NoOpToBackend
    | RequestData
    | GridChange (List.Nonempty.Nonempty LocalChange)
    | UserRename String


type BackendMsg
    = NoOpBackendMsg
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId


type alias LoadingData_ = 
    { user : Evergreen.V3.User.User
    , grid : Evergreen.V3.Grid.Grid
    , otherUsers : (List Evergreen.V3.User.User)
    , undoHistory : (List (Dict.Dict (Int, Int) Int))
    , redoHistory : (List (Dict.Dict (Int, Int) Int))
    }


type ToFrontend
    = NoOpToFrontend
    | LoadingData LoadingData_
    | ServerChangeBroadcast (List.Nonempty.Nonempty ServerChange)
    | LocalChangeResponse (List.Nonempty.Nonempty LocalChange)
    | NewUserBroadcast Evergreen.V3.User.User
    | UserModifiedBroadcast Evergreen.V3.User.User