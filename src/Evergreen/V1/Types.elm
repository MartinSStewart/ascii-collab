module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V1.Cursor
import Dict
import Evergreen.V1.Grid
import Evergreen.V1.Helper
import Html.Events.Extra.Mouse
import Keyboard
import Lamdera
import List.Nonempty
import Evergreen.V1.LocalModel
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


type alias FrontendLoading = 
    { key : Browser.Navigation.Key
    , windowSize : (Evergreen.V1.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V1.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    }


type LocalChange
    = LocalGridChange Evergreen.V1.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | AddUndo


type ServerChange
    = ServerGridChange Evergreen.V1.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V1.User.UserId
    , undoPoints : (Dict.Dict (Int, Int) Int)
    }


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange


type alias LocalGrid = 
    { grid : Evergreen.V1.Grid.Grid
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
    { start : (Evergreen.V1.Point2d.Point2d Pixels.Pixels Evergreen.V1.Units.ScreenCoordinate)
    , start_ : (Evergreen.V1.Point2d.Point2d Evergreen.V1.Units.WorldPixel Evergreen.V1.Units.WorldCoordinate)
    , current : (Evergreen.V1.Point2d.Point2d Pixels.Pixels Evergreen.V1.Units.ScreenCoordinate)
    }


type ToolType
    = DragTool
    | RectangleTool
    | SelectTool


type alias FrontendLoaded = 
    { key : Browser.Navigation.Key
    , localModel : (Evergreen.V1.LocalModel.LocalModel Change LocalGrid)
    , meshes : (Dict.Dict (Int, Int) (WebGL.Mesh Vertex))
    , cursorMesh : (WebGL.Mesh 
    { position : Math.Vector2.Vec2
    })
    , viewPoint : (Evergreen.V1.Point2d.Point2d Evergreen.V1.Units.WorldPixel Evergreen.V1.Units.WorldCoordinate)
    , cursor : Evergreen.V1.Cursor.Cursor
    , texture : (Maybe WebGL.Texture.Texture)
    , pressedKeys : (List Keyboard.Key)
    , windowSize : (Evergreen.V1.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V1.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , mouseMiddle : MouseButtonState
    , user : Evergreen.V1.User.User
    , otherUsers : (List Evergreen.V1.User.User)
    , pendingChanges : (List LocalChange)
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendModel =
    { grid : Evergreen.V1.Grid.Grid
    , undoPoints : (Dict.Dict Evergreen.V1.User.RawUserId 
    { undoHistory : (List (Dict.Dict (Int, Int) Int))
    , redoHistory : (List (Dict.Dict (Int, Int) Int))
    })
    , userSessions : (Set.Set (Lamdera.SessionId, Lamdera.ClientId))
    , users : (Dict.Dict Lamdera.SessionId Evergreen.V1.User.User)
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
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V1.Point2d.Point2d Pixels.Pixels Evergreen.V1.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V1.Point2d.Point2d Pixels.Pixels Evergreen.V1.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V1.Point2d.Point2d Pixels.Pixels Evergreen.V1.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed


type ToBackend
    = NoOpToBackend
    | RequestData
    | GridChange (List.Nonempty.Nonempty LocalChange)
    | UserRename String


type BackendMsg
    = NoOpBackendMsg
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId


type alias LoadingData_ = 
    { user : Evergreen.V1.User.User
    , grid : Evergreen.V1.Grid.Grid
    , otherUsers : (List Evergreen.V1.User.User)
    , undoHistory : (List (Dict.Dict (Int, Int) Int))
    , redoHistory : (List (Dict.Dict (Int, Int) Int))
    }


type ToFrontend
    = NoOpToFrontend
    | LoadingData LoadingData_
    | ServerChangeBroadcast (List.Nonempty.Nonempty ServerChange)
    | LocalChangeResponse (List.Nonempty.Nonempty LocalChange)
    | NewUserBroadcast Evergreen.V1.User.User
    | UserModifiedBroadcast Evergreen.V1.User.User