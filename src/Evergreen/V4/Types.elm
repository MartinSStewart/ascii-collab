module Evergreen.V4.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V4.Cursor
import Dict
import EverySet
import Evergreen.V4.Grid
import Evergreen.V4.Helper
import Html.Events.Extra.Mouse
import Keyboard
import Lamdera
import List.Nonempty
import Evergreen.V4.LocalModel
import Math.Vector2
import Pixels
import Evergreen.V4.Point2d
import Quantity
import Set
import Time
import Evergreen.V4.Units
import Url
import Evergreen.V4.User
import WebGL
import WebGL.Texture


type alias FrontendLoading = 
    { key : Browser.Navigation.Key
    , windowSize : (Evergreen.V4.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V4.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    }


type LocalChange
    = LocalGridChange Evergreen.V4.Grid.LocalChange
    | LocalUndo
    | LocalRedo
    | LocalAddUndo
    | LocalToggleUserVisibility Evergreen.V4.User.UserId


type ServerChange
    = ServerGridChange Evergreen.V4.Grid.Change
    | ServerUndoPoint 
    { userId : Evergreen.V4.User.UserId
    , undoPoints : (Dict.Dict (Int, Int) Int)
    }
    | ServerUserNew (Evergreen.V4.User.UserId, Evergreen.V4.User.UserData)


type Change
    = LocalChange LocalChange
    | ServerChange ServerChange


type alias LocalGrid = 
    { grid : Evergreen.V4.Grid.Grid
    , undoHistory : (List (Dict.Dict (Int, Int) Int))
    , redoHistory : (List (Dict.Dict (Int, Int) Int))
    , user : (Evergreen.V4.User.UserId, Evergreen.V4.User.UserData)
    , otherUsers : (List (Evergreen.V4.User.UserId, Evergreen.V4.User.UserData))
    , hiddenUsers : (EverySet.EverySet Evergreen.V4.User.UserId)
    }


type MouseButtonState
    = MouseButtonUp
    | MouseButtonDown 
    { start : (Evergreen.V4.Point2d.Point2d Pixels.Pixels Evergreen.V4.Units.ScreenCoordinate)
    , start_ : (Evergreen.V4.Point2d.Point2d Evergreen.V4.Units.WorldPixel Evergreen.V4.Units.WorldCoordinate)
    , current : (Evergreen.V4.Point2d.Point2d Pixels.Pixels Evergreen.V4.Units.ScreenCoordinate)
    }


type ToolType
    = DragTool
    | SelectTool
    | HideUserTool (Maybe Evergreen.V4.User.UserId)


type alias FrontendLoaded = 
    { key : Browser.Navigation.Key
    , localModel : (Evergreen.V4.LocalModel.LocalModel Change LocalGrid)
    , meshes : (Dict.Dict (Int, Int) (WebGL.Mesh Evergreen.V4.Grid.Vertex))
    , cursorMesh : (WebGL.Mesh 
    { position : Math.Vector2.Vec2
    })
    , viewPoint : (Evergreen.V4.Point2d.Point2d Evergreen.V4.Units.WorldPixel Evergreen.V4.Units.WorldCoordinate)
    , cursor : Evergreen.V4.Cursor.Cursor
    , texture : (Maybe WebGL.Texture.Texture)
    , pressedKeys : (List Keyboard.Key)
    , windowSize : (Evergreen.V4.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V4.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , mouseMiddle : MouseButtonState
    , pendingChanges : (List LocalChange)
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : (Maybe Time.Posix)
    , userPressHighlighted : (Maybe Evergreen.V4.User.UserId)
    , userHoverHighlighted : (Maybe Evergreen.V4.User.UserId)
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendModel =
    { grid : Evergreen.V4.Grid.Grid
    , undoPoints : (Dict.Dict Evergreen.V4.User.RawUserId 
    { undoHistory : (List (Dict.Dict (Int, Int) Int))
    , redoHistory : (List (Dict.Dict (Int, Int) Int))
    })
    , userSessions : (Dict.Dict Lamdera.SessionId 
    { clientIds : (Set.Set Lamdera.ClientId)
    , userId : Evergreen.V4.User.UserId
    })
    , users : (Dict.Dict Evergreen.V4.User.RawUserId 
    { userData : Evergreen.V4.User.UserData
    , hiddenUsers : (EverySet.EverySet Evergreen.V4.User.UserId)
    })
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V4.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V4.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V4.Point2d.Point2d Pixels.Pixels Evergreen.V4.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V4.Point2d.Point2d Pixels.Pixels Evergreen.V4.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V4.Point2d.Point2d Pixels.Pixels Evergreen.V4.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V4.Point2d.Point2d Pixels.Pixels Evergreen.V4.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V4.User.UserId
    | UserColorSquarePressed Evergreen.V4.User.UserId
    | UserTagMouseEntered Evergreen.V4.User.UserId
    | UserTagMouseExited Evergreen.V4.User.UserId


type ToBackend
    = NoOpToBackend
    | RequestData
    | GridChange (List.Nonempty.Nonempty LocalChange)


type BackendMsg
    = NoOpBackendMsg
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId


type alias LoadingData_ = 
    { user : (Evergreen.V4.User.UserId, Evergreen.V4.User.UserData)
    , grid : Evergreen.V4.Grid.Grid
    , otherUsers : (List (Evergreen.V4.User.UserId, Evergreen.V4.User.UserData))
    , hiddenUsers : (EverySet.EverySet Evergreen.V4.User.UserId)
    , undoHistory : (List (Dict.Dict (Int, Int) Int))
    , redoHistory : (List (Dict.Dict (Int, Int) Int))
    }


type ToFrontend
    = NoOpToFrontend
    | LoadingData LoadingData_
    | ServerChangeBroadcast (List.Nonempty.Nonempty ServerChange)
    | LocalChangeResponse (List.Nonempty.Nonempty LocalChange)