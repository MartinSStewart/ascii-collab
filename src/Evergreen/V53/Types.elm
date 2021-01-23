module Evergreen.V53.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Duration
import Email
import Evergreen.V53.Bounds
import Evergreen.V53.Change
import Evergreen.V53.Cursor
import Evergreen.V53.Grid
import Evergreen.V53.Helper
import Evergreen.V53.LocalGrid
import Evergreen.V53.LocalModel
import Evergreen.V53.NotifyMe
import Evergreen.V53.Point2d
import Evergreen.V53.RecentChanges
import Evergreen.V53.Units
import Evergreen.V53.UrlHelper
import Evergreen.V53.User
import EverySet
import Html.Events.Extra.Mouse
import Keyboard
import Lamdera
import List.Nonempty
import Math.Vector2
import Pixels
import Quantity
import SendGrid
import Time
import Url
import WebGL
import WebGL.Texture


type alias FrontendLoading = 
    { key : Browser.Navigation.Key
    , windowSize : (Evergreen.V53.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V53.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : (Evergreen.V53.Helper.Coord Evergreen.V53.Units.AsciiUnit)
    , mousePosition : (Evergreen.V53.Point2d.Point2d Pixels.Pixels Evergreen.V53.Units.ScreenCoordinate)
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V53.NotifyMe.Model
    }


type MouseButtonState
    = MouseButtonUp 
    { current : (Evergreen.V53.Point2d.Point2d Pixels.Pixels Evergreen.V53.Units.ScreenCoordinate)
    }
    | MouseButtonDown 
    { start : (Evergreen.V53.Point2d.Point2d Pixels.Pixels Evergreen.V53.Units.ScreenCoordinate)
    , start_ : (Evergreen.V53.Point2d.Point2d Evergreen.V53.Units.WorldPixel Evergreen.V53.Units.WorldCoordinate)
    , current : (Evergreen.V53.Point2d.Point2d Pixels.Pixels Evergreen.V53.Units.ScreenCoordinate)
    }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe (Evergreen.V53.User.UserId, (Evergreen.V53.Helper.Coord Evergreen.V53.Units.AsciiUnit)))


type alias FrontendLoaded = 
    { key : Browser.Navigation.Key
    , localModel : (Evergreen.V53.LocalModel.LocalModel Evergreen.V53.Change.Change Evergreen.V53.LocalGrid.LocalGrid)
    , meshes : (Dict.Dict Evergreen.V53.Helper.RawCellCoord (WebGL.Mesh Evergreen.V53.Grid.Vertex))
    , cursorMesh : (WebGL.Mesh 
    { position : Math.Vector2.Vec2
    })
    , viewPoint : (Evergreen.V53.Point2d.Point2d Evergreen.V53.Units.WorldPixel Evergreen.V53.Units.WorldCoordinate)
    , viewPointLastInterval : (Evergreen.V53.Point2d.Point2d Evergreen.V53.Units.WorldPixel Evergreen.V53.Units.WorldCoordinate)
    , cursor : Evergreen.V53.Cursor.Cursor
    , texture : (Maybe WebGL.Texture.Texture)
    , pressedKeys : (List Keyboard.Key)
    , windowSize : (Evergreen.V53.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V53.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : (Maybe (Time.Posix, (Evergreen.V53.Point2d.Point2d Pixels.Pixels Evergreen.V53.Units.ScreenCoordinate)))
    , mouseMiddle : MouseButtonState
    , pendingChanges : (List Evergreen.V53.Change.LocalChange)
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : (Maybe Time.Posix)
    , userHoverHighlighted : (Maybe Evergreen.V53.User.UserId)
    , highlightContextMenu : (Maybe 
    { userId : Evergreen.V53.User.UserId
    , hidePoint : (Evergreen.V53.Helper.Coord Evergreen.V53.Units.AsciiUnit)
    })
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V53.NotifyMe.Model
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData = 
    { hiddenUsers : (EverySet.EverySet Evergreen.V53.User.UserId)
    , hiddenForAll : Bool
    , undoHistory : (List (Dict.Dict Evergreen.V53.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V53.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V53.Helper.RawCellCoord Int)
    }


type alias SubscribedEmail = 
    { email : Email.Email
    , frequency : Evergreen.V53.NotifyMe.Frequency
    , confirmTime : Time.Posix
    , userId : Evergreen.V53.User.UserId
    , unsubscribeKey : Evergreen.V53.UrlHelper.UnsubscribeEmailKey
    }


type alias PendingEmail = 
    { email : Email.Email
    , frequency : Evergreen.V53.NotifyMe.Frequency
    , creationTime : Time.Posix
    , userId : Evergreen.V53.User.UserId
    , key : Evergreen.V53.UrlHelper.ConfirmEmailKey
    }


type BackendError
    = SendGridError Email.Email SendGrid.Error


type alias BackendModel =
    { grid : Evergreen.V53.Grid.Grid
    , userSessions : (Dict.Dict Lamdera.SessionId 
    { clientIds : (Dict.Dict Lamdera.ClientId (Evergreen.V53.Bounds.Bounds Evergreen.V53.Units.CellUnit))
    , userId : Evergreen.V53.User.UserId
    })
    , users : (Dict.Dict Evergreen.V53.User.RawUserId BackendUserData)
    , usersHiddenRecently : (List 
    { reporter : Evergreen.V53.User.UserId
    , hiddenUser : Evergreen.V53.User.UserId
    , hidePoint : (Evergreen.V53.Helper.Coord Evergreen.V53.Units.AsciiUnit)
    })
    , userChangesRecently : Evergreen.V53.RecentChanges.RecentChanges
    , subscribedEmails : (List SubscribedEmail)
    , pendingEmails : (List PendingEmail)
    , secretLinkCounter : Int
    , errors : (List (Time.Posix, BackendError))
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V53.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V53.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V53.Point2d.Point2d Pixels.Pixels Evergreen.V53.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V53.Point2d.Point2d Pixels.Pixels Evergreen.V53.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V53.Point2d.Point2d Pixels.Pixels Evergreen.V53.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V53.Point2d.Point2d Pixels.Pixels Evergreen.V53.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V53.User.UserId
    | UserTagMouseEntered Evergreen.V53.User.UserId
    | UserTagMouseExited Evergreen.V53.User.UserId
    | HideForAllTogglePressed Evergreen.V53.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed 
    { userId : Evergreen.V53.User.UserId
    , hidePoint : (Evergreen.V53.Helper.Coord Evergreen.V53.Units.AsciiUnit)
    }
    | AnimationFrame Time.Posix
    | PressedCancelNotifyMe
    | PressedSubmitNotifyMe Evergreen.V53.NotifyMe.Validated
    | NotifyMeModelChanged Evergreen.V53.NotifyMe.Model


type EmailEvent
    = ConfirmationEmailConfirmed_ Evergreen.V53.UrlHelper.ConfirmEmailKey
    | UnsubscribeEmail Evergreen.V53.UrlHelper.UnsubscribeEmailKey


type ToBackend
    = ConnectToBackend (Evergreen.V53.Bounds.Bounds Evergreen.V53.Units.CellUnit) (Maybe EmailEvent)
    | GridChange (List.Nonempty.Nonempty Evergreen.V53.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V53.Bounds.Bounds Evergreen.V53.Units.CellUnit)
    | NotifyMeSubmitted Evergreen.V53.NotifyMe.Validated


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent
    | ConfirmationEmailSent Lamdera.SessionId Time.Posix (Result SendGrid.Error ())
    | ChangeEmailSent Time.Posix Email.Email (Result SendGrid.Error ())
    | UpdateFromFrontend Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix


type alias LoadingData_ = 
    { user : Evergreen.V53.User.UserId
    , grid : Evergreen.V53.Grid.Grid
    , hiddenUsers : (EverySet.EverySet Evergreen.V53.User.UserId)
    , adminHiddenUsers : (EverySet.EverySet Evergreen.V53.User.UserId)
    , undoHistory : (List (Dict.Dict Evergreen.V53.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V53.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V53.Helper.RawCellCoord Int)
    , viewBounds : (Evergreen.V53.Bounds.Bounds Evergreen.V53.Units.CellUnit)
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V53.Change.Change)
    | NotifyMeEmailSent 
    { isSuccessful : Bool
    }
    | NotifyMeConfirmed
    | UnsubscribeEmailConfirmed