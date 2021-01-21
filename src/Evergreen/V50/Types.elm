module Evergreen.V50.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Duration
import Email
import Evergreen.V50.Bounds
import Evergreen.V50.Change
import Evergreen.V50.Cursor
import Evergreen.V50.Grid
import Evergreen.V50.Helper
import Evergreen.V50.LocalGrid
import Evergreen.V50.LocalModel
import Evergreen.V50.NotifyMe
import Evergreen.V50.Point2d
import Evergreen.V50.RecentChanges
import Evergreen.V50.Units
import Evergreen.V50.UrlHelper
import Evergreen.V50.User
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
    , windowSize : (Evergreen.V50.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V50.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : (Evergreen.V50.Helper.Coord Evergreen.V50.Units.AsciiUnit)
    , mousePosition : (Evergreen.V50.Point2d.Point2d Pixels.Pixels Evergreen.V50.Units.ScreenCoordinate)
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V50.NotifyMe.Model
    }


type MouseButtonState
    = MouseButtonUp 
    { current : (Evergreen.V50.Point2d.Point2d Pixels.Pixels Evergreen.V50.Units.ScreenCoordinate)
    }
    | MouseButtonDown 
    { start : (Evergreen.V50.Point2d.Point2d Pixels.Pixels Evergreen.V50.Units.ScreenCoordinate)
    , start_ : (Evergreen.V50.Point2d.Point2d Evergreen.V50.Units.WorldPixel Evergreen.V50.Units.WorldCoordinate)
    , current : (Evergreen.V50.Point2d.Point2d Pixels.Pixels Evergreen.V50.Units.ScreenCoordinate)
    }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe (Evergreen.V50.User.UserId, (Evergreen.V50.Helper.Coord Evergreen.V50.Units.AsciiUnit)))


type alias FrontendLoaded = 
    { key : Browser.Navigation.Key
    , localModel : (Evergreen.V50.LocalModel.LocalModel Evergreen.V50.Change.Change Evergreen.V50.LocalGrid.LocalGrid)
    , meshes : (Dict.Dict Evergreen.V50.Helper.RawCellCoord (WebGL.Mesh Evergreen.V50.Grid.Vertex))
    , cursorMesh : (WebGL.Mesh 
    { position : Math.Vector2.Vec2
    })
    , viewPoint : (Evergreen.V50.Point2d.Point2d Evergreen.V50.Units.WorldPixel Evergreen.V50.Units.WorldCoordinate)
    , viewPointLastInterval : (Evergreen.V50.Point2d.Point2d Evergreen.V50.Units.WorldPixel Evergreen.V50.Units.WorldCoordinate)
    , cursor : Evergreen.V50.Cursor.Cursor
    , texture : (Maybe WebGL.Texture.Texture)
    , pressedKeys : (List Keyboard.Key)
    , windowSize : (Evergreen.V50.Helper.Coord Pixels.Pixels)
    , devicePixelRatio : (Quantity.Quantity Float (Quantity.Rate Evergreen.V50.Units.WorldPixel Pixels.Pixels))
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : (Maybe (Time.Posix, (Evergreen.V50.Point2d.Point2d Pixels.Pixels Evergreen.V50.Units.ScreenCoordinate)))
    , mouseMiddle : MouseButtonState
    , pendingChanges : (List Evergreen.V50.Change.LocalChange)
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : (Maybe Time.Posix)
    , userHoverHighlighted : (Maybe Evergreen.V50.User.UserId)
    , highlightContextMenu : (Maybe 
    { userId : Evergreen.V50.User.UserId
    , hidePoint : (Evergreen.V50.Helper.Coord Evergreen.V50.Units.AsciiUnit)
    })
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V50.NotifyMe.Model
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData = 
    { hiddenUsers : (EverySet.EverySet Evergreen.V50.User.UserId)
    , hiddenForAll : Bool
    , undoHistory : (List (Dict.Dict Evergreen.V50.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V50.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V50.Helper.RawCellCoord Int)
    }


type alias SubscribedEmail = 
    { email : Email.Email
    , frequency : Evergreen.V50.NotifyMe.Frequency
    , confirmTime : Time.Posix
    , userId : Evergreen.V50.User.UserId
    , unsubscribeKey : Evergreen.V50.UrlHelper.UnsubscribeEmailKey
    }


type alias PendingEmail = 
    { email : Email.Email
    , frequency : Evergreen.V50.NotifyMe.Frequency
    , creationTime : Time.Posix
    , userId : Evergreen.V50.User.UserId
    , key : Evergreen.V50.UrlHelper.ConfirmEmailKey
    }


type BackendError
    = SendGridError Email.Email SendGrid.Error


type alias BackendModel =
    { grid : Evergreen.V50.Grid.Grid
    , userSessions : (Dict.Dict Lamdera.SessionId 
    { clientIds : (Dict.Dict Lamdera.ClientId (Evergreen.V50.Bounds.Bounds Evergreen.V50.Units.CellUnit))
    , userId : Evergreen.V50.User.UserId
    })
    , users : (Dict.Dict Evergreen.V50.User.RawUserId BackendUserData)
    , usersHiddenRecently : (List 
    { reporter : Evergreen.V50.User.UserId
    , hiddenUser : Evergreen.V50.User.UserId
    , hidePoint : (Evergreen.V50.Helper.Coord Evergreen.V50.Units.AsciiUnit)
    })
    , userChangesRecently : Evergreen.V50.RecentChanges.RecentChanges
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
    | WindowResized (Evergreen.V50.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V50.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V50.Point2d.Point2d Pixels.Pixels Evergreen.V50.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V50.Point2d.Point2d Pixels.Pixels Evergreen.V50.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V50.Point2d.Point2d Pixels.Pixels Evergreen.V50.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V50.Point2d.Point2d Pixels.Pixels Evergreen.V50.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V50.User.UserId
    | UserTagMouseEntered Evergreen.V50.User.UserId
    | UserTagMouseExited Evergreen.V50.User.UserId
    | HideForAllTogglePressed Evergreen.V50.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed 
    { userId : Evergreen.V50.User.UserId
    , hidePoint : (Evergreen.V50.Helper.Coord Evergreen.V50.Units.AsciiUnit)
    }
    | AnimationFrame Time.Posix
    | PressedCancelNotifyMe
    | PressedSubmitNotifyMe Evergreen.V50.NotifyMe.Validated
    | NotifyMeModelChanged Evergreen.V50.NotifyMe.Model


type EmailEvent
    = ConfirmationEmailConfirmed_ Evergreen.V50.UrlHelper.ConfirmEmailKey
    | UnsubscribeEmail Evergreen.V50.UrlHelper.UnsubscribeEmailKey


type ToBackend
    = ConnectToBackend (Evergreen.V50.Bounds.Bounds Evergreen.V50.Units.CellUnit) (Maybe EmailEvent)
    | GridChange (List.Nonempty.Nonempty Evergreen.V50.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V50.Bounds.Bounds Evergreen.V50.Units.CellUnit)
    | NotifyMeSubmitted Evergreen.V50.NotifyMe.Validated


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent
    | ConfirmationEmailSent Lamdera.SessionId Time.Posix (Result SendGrid.Error ())
    | ChangeEmailSent Time.Posix Email.Email (Result SendGrid.Error ())
    | UpdateFromFrontend Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix


type alias LoadingData_ = 
    { user : Evergreen.V50.User.UserId
    , grid : Evergreen.V50.Grid.Grid
    , hiddenUsers : (EverySet.EverySet Evergreen.V50.User.UserId)
    , adminHiddenUsers : (EverySet.EverySet Evergreen.V50.User.UserId)
    , undoHistory : (List (Dict.Dict Evergreen.V50.Helper.RawCellCoord Int))
    , redoHistory : (List (Dict.Dict Evergreen.V50.Helper.RawCellCoord Int))
    , undoCurrent : (Dict.Dict Evergreen.V50.Helper.RawCellCoord Int)
    , viewBounds : (Evergreen.V50.Bounds.Bounds Evergreen.V50.Units.CellUnit)
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V50.Change.Change)
    | NotifyMeEmailSent 
    { isSuccessful : Bool
    }
    | NotifyMeConfirmed
    | UnsubscribeEmailConfirmed