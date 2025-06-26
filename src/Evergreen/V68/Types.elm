module Evergreen.V68.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Duration
import EmailAddress
import Evergreen.V68.Bounds
import Evergreen.V68.Change
import Evergreen.V68.Cursor
import Evergreen.V68.Grid
import Evergreen.V68.Helper
import Evergreen.V68.LocalGrid
import Evergreen.V68.LocalModel
import Evergreen.V68.NotifyMe
import Evergreen.V68.Point2d
import Evergreen.V68.RecentChanges
import Evergreen.V68.Units
import Evergreen.V68.UrlHelper
import Evergreen.V68.User
import Html.Events.Extra.Mouse
import Keyboard
import Lamdera
import List.Nonempty
import Math.Vector2
import Pixels
import Quantity
import SendGrid
import SeqSet
import Time
import Url
import WebGL
import WebGL.Texture


type alias FrontendLoading =
    { key : Browser.Navigation.Key
    , windowSize : Evergreen.V68.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V68.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V68.Helper.Coord Evergreen.V68.Units.AsciiUnit
    , mousePosition : Evergreen.V68.Point2d.Point2d Pixels.Pixels Evergreen.V68.Units.ScreenCoordinate
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V68.NotifyMe.Model
    }


type MouseButtonState
    = MouseButtonUp
        { current : Evergreen.V68.Point2d.Point2d Pixels.Pixels Evergreen.V68.Units.ScreenCoordinate
        }
    | MouseButtonDown
        { start : Evergreen.V68.Point2d.Point2d Pixels.Pixels Evergreen.V68.Units.ScreenCoordinate
        , start_ : Evergreen.V68.Point2d.Point2d Evergreen.V68.Units.WorldPixel Evergreen.V68.Units.WorldCoordinate
        , current : Evergreen.V68.Point2d.Point2d Pixels.Pixels Evergreen.V68.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe ( Evergreen.V68.User.UserId, Evergreen.V68.Helper.Coord Evergreen.V68.Units.AsciiUnit ))


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V68.LocalModel.LocalModel Evergreen.V68.Change.Change Evergreen.V68.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V68.Helper.RawCellCoord (WebGL.Mesh Evergreen.V68.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V68.Point2d.Point2d Evergreen.V68.Units.WorldPixel Evergreen.V68.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V68.Point2d.Point2d Evergreen.V68.Units.WorldPixel Evergreen.V68.Units.WorldCoordinate
    , cursor : Evergreen.V68.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V68.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V68.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : Maybe ( Time.Posix, Evergreen.V68.Point2d.Point2d Pixels.Pixels Evergreen.V68.Units.ScreenCoordinate )
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V68.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userHoverHighlighted : Maybe Evergreen.V68.User.UserId
    , highlightContextMenu :
        Maybe
            { userId : Evergreen.V68.User.UserId
            , hidePoint : Evergreen.V68.Helper.Coord Evergreen.V68.Units.AsciiUnit
            }
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V68.NotifyMe.Model
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData =
    { hiddenUsers : SeqSet.SeqSet Evergreen.V68.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V68.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V68.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V68.Helper.RawCellCoord Int
    }


type alias SubscribedEmail =
    { email : EmailAddress.EmailAddress
    , frequency : Evergreen.V68.NotifyMe.Frequency
    , confirmTime : Time.Posix
    , userId : Evergreen.V68.User.UserId
    , unsubscribeKey : Evergreen.V68.UrlHelper.UnsubscribeEmailKey
    }


type alias PendingEmail =
    { email : EmailAddress.EmailAddress
    , frequency : Evergreen.V68.NotifyMe.Frequency
    , creationTime : Time.Posix
    , userId : Evergreen.V68.User.UserId
    , key : Evergreen.V68.UrlHelper.ConfirmEmailKey
    }


type BackendError
    = SendGridError EmailAddress.EmailAddress SendGrid.Error


type alias BackendModel =
    { grid : Evergreen.V68.Grid.Grid
    , userSessions :
        Dict.Dict
            Lamdera.SessionId
            { clientIds : Dict.Dict Lamdera.ClientId (Evergreen.V68.Bounds.Bounds Evergreen.V68.Units.CellUnit)
            , userId : Evergreen.V68.User.UserId
            }
    , users : Dict.Dict Evergreen.V68.User.RawUserId BackendUserData
    , usersHiddenRecently :
        List
            { reporter : Evergreen.V68.User.UserId
            , hiddenUser : Evergreen.V68.User.UserId
            , hidePoint : Evergreen.V68.Helper.Coord Evergreen.V68.Units.AsciiUnit
            }
    , userChangesRecently : Evergreen.V68.RecentChanges.RecentChanges
    , subscribedEmails : List SubscribedEmail
    , pendingEmails : List PendingEmail
    , secretLinkCounter : Int
    , errors : List ( Time.Posix, BackendError )
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V68.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V68.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V68.Point2d.Point2d Pixels.Pixels Evergreen.V68.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V68.Point2d.Point2d Pixels.Pixels Evergreen.V68.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V68.Point2d.Point2d Pixels.Pixels Evergreen.V68.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V68.Point2d.Point2d Pixels.Pixels Evergreen.V68.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V68.User.UserId
    | UserTagMouseEntered Evergreen.V68.User.UserId
    | UserTagMouseExited Evergreen.V68.User.UserId
    | HideForAllTogglePressed Evergreen.V68.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed
        { userId : Evergreen.V68.User.UserId
        , hidePoint : Evergreen.V68.Helper.Coord Evergreen.V68.Units.AsciiUnit
        }
    | AnimationFrame Time.Posix
    | PressedCancelNotifyMe
    | PressedSubmitNotifyMe Evergreen.V68.NotifyMe.Validated
    | NotifyMeModelChanged Evergreen.V68.NotifyMe.Model


type EmailEvent
    = ConfirmationEmailConfirmed_ Evergreen.V68.UrlHelper.ConfirmEmailKey
    | UnsubscribeEmail Evergreen.V68.UrlHelper.UnsubscribeEmailKey


type ToBackend
    = ConnectToBackend (Evergreen.V68.Bounds.Bounds Evergreen.V68.Units.CellUnit) (Maybe EmailEvent)
    | GridChange (List.Nonempty.Nonempty Evergreen.V68.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V68.Bounds.Bounds Evergreen.V68.Units.CellUnit)
    | NotifyMeSubmitted Evergreen.V68.NotifyMe.Validated


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent
    | ConfirmationEmailSent Lamdera.SessionId Time.Posix (Result SendGrid.Error ())
    | ChangeEmailSent Time.Posix EmailAddress.EmailAddress (Result SendGrid.Error ())
    | UpdateFromFrontend Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix


type alias LoadingData_ =
    { user : Evergreen.V68.User.UserId
    , grid : Evergreen.V68.Grid.Grid
    , hiddenUsers : SeqSet.SeqSet Evergreen.V68.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V68.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V68.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V68.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V68.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V68.Bounds.Bounds Evergreen.V68.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V68.Change.Change)
    | NotifyMeEmailSent
        { isSuccessful : Bool
        }
    | NotifyMeConfirmed
    | UnsubscribeEmailConfirmed
