module Evergreen.V77.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Duration
import EmailAddress
import Evergreen.V77.Bounds
import Evergreen.V77.Change
import Evergreen.V77.Cursor
import Evergreen.V77.Grid
import Evergreen.V77.Helper
import Evergreen.V77.LocalGrid
import Evergreen.V77.LocalModel
import Evergreen.V77.NotifyMe
import Evergreen.V77.Point2d
import Evergreen.V77.RecentChanges
import Evergreen.V77.Units
import Evergreen.V77.UrlHelper
import Evergreen.V77.User
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
    , windowSize : Evergreen.V77.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V77.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V77.Helper.Coord Evergreen.V77.Units.AsciiUnit
    , mousePosition : Evergreen.V77.Point2d.Point2d Pixels.Pixels Evergreen.V77.Units.ScreenCoordinate
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V77.NotifyMe.Model
    }


type MouseButtonState
    = MouseButtonUp
        { current : Evergreen.V77.Point2d.Point2d Pixels.Pixels Evergreen.V77.Units.ScreenCoordinate
        }
    | MouseButtonDown
        { start : Evergreen.V77.Point2d.Point2d Pixels.Pixels Evergreen.V77.Units.ScreenCoordinate
        , start_ : Evergreen.V77.Point2d.Point2d Evergreen.V77.Units.WorldPixel Evergreen.V77.Units.WorldCoordinate
        , current : Evergreen.V77.Point2d.Point2d Pixels.Pixels Evergreen.V77.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe ( Evergreen.V77.User.UserId, Evergreen.V77.Helper.Coord Evergreen.V77.Units.AsciiUnit ))


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V77.LocalModel.LocalModel Evergreen.V77.Change.Change Evergreen.V77.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V77.Helper.RawCellCoord (WebGL.Mesh Evergreen.V77.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V77.Point2d.Point2d Evergreen.V77.Units.WorldPixel Evergreen.V77.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V77.Point2d.Point2d Evergreen.V77.Units.WorldPixel Evergreen.V77.Units.WorldCoordinate
    , cursor : Evergreen.V77.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V77.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V77.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : Maybe ( Time.Posix, Evergreen.V77.Point2d.Point2d Pixels.Pixels Evergreen.V77.Units.ScreenCoordinate )
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V77.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userHoverHighlighted : Maybe Evergreen.V77.User.UserId
    , highlightContextMenu :
        Maybe
            { userId : Evergreen.V77.User.UserId
            , hidePoint : Evergreen.V77.Helper.Coord Evergreen.V77.Units.AsciiUnit
            }
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V77.NotifyMe.Model
    , textAreaText : String
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData =
    { hiddenUsers : SeqSet.SeqSet Evergreen.V77.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V77.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V77.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V77.Helper.RawCellCoord Int
    }


type alias SubscribedEmail =
    { email : EmailAddress.EmailAddress
    , frequency : Evergreen.V77.NotifyMe.Frequency
    , confirmTime : Time.Posix
    , userId : Evergreen.V77.User.UserId
    , unsubscribeKey : Evergreen.V77.UrlHelper.UnsubscribeEmailKey
    }


type alias PendingEmail =
    { email : EmailAddress.EmailAddress
    , frequency : Evergreen.V77.NotifyMe.Frequency
    , creationTime : Time.Posix
    , userId : Evergreen.V77.User.UserId
    , key : Evergreen.V77.UrlHelper.ConfirmEmailKey
    }


type BackendError
    = SendGridError EmailAddress.EmailAddress SendGrid.Error


type alias BackendModel =
    { grid : Evergreen.V77.Grid.Grid
    , userSessions :
        Dict.Dict
            Lamdera.SessionId
            { clientIds : Dict.Dict Lamdera.ClientId (Evergreen.V77.Bounds.Bounds Evergreen.V77.Units.CellUnit)
            , userId : Evergreen.V77.User.UserId
            }
    , users : Dict.Dict Evergreen.V77.User.RawUserId BackendUserData
    , usersHiddenRecently :
        List
            { reporter : Evergreen.V77.User.UserId
            , hiddenUser : Evergreen.V77.User.UserId
            , hidePoint : Evergreen.V77.Helper.Coord Evergreen.V77.Units.AsciiUnit
            }
    , userChangesRecently : Evergreen.V77.RecentChanges.RecentChanges
    , subscribedEmails : List SubscribedEmail
    , pendingEmails : List PendingEmail
    , secretLinkCounter : Int
    , errors : List ( Time.Posix, BackendError )
    , dummyField : ()
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V77.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V77.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | TextAreaFocused
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V77.Point2d.Point2d Pixels.Pixels Evergreen.V77.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V77.Point2d.Point2d Pixels.Pixels Evergreen.V77.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V77.Point2d.Point2d Pixels.Pixels Evergreen.V77.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V77.Point2d.Point2d Pixels.Pixels Evergreen.V77.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V77.User.UserId
    | UserTagMouseEntered Evergreen.V77.User.UserId
    | UserTagMouseExited Evergreen.V77.User.UserId
    | HideForAllTogglePressed Evergreen.V77.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed
        { userId : Evergreen.V77.User.UserId
        , hidePoint : Evergreen.V77.Helper.Coord Evergreen.V77.Units.AsciiUnit
        }
    | AnimationFrame Time.Posix
    | PressedCancelNotifyMe
    | PressedSubmitNotifyMe Evergreen.V77.NotifyMe.Validated
    | NotifyMeModelChanged Evergreen.V77.NotifyMe.Model


type EmailEvent
    = ConfirmationEmailConfirmed_ Evergreen.V77.UrlHelper.ConfirmEmailKey
    | UnsubscribeEmail Evergreen.V77.UrlHelper.UnsubscribeEmailKey


type ToBackend
    = ConnectToBackend (Evergreen.V77.Bounds.Bounds Evergreen.V77.Units.CellUnit) (Maybe EmailEvent)
    | GridChange (List.Nonempty.Nonempty Evergreen.V77.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V77.Bounds.Bounds Evergreen.V77.Units.CellUnit)
    | NotifyMeSubmitted Evergreen.V77.NotifyMe.Validated


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent
    | ConfirmationEmailSent Lamdera.SessionId Time.Posix (Result SendGrid.Error ())
    | ChangeEmailSent Time.Posix EmailAddress.EmailAddress (Result SendGrid.Error ())
    | UpdateFromFrontend Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix


type alias LoadingData_ =
    { user : Evergreen.V77.User.UserId
    , grid : Evergreen.V77.Grid.Grid
    , hiddenUsers : SeqSet.SeqSet Evergreen.V77.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V77.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V77.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V77.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V77.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V77.Bounds.Bounds Evergreen.V77.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V77.Change.Change)
    | NotifyMeEmailSent
        { isSuccessful : Bool
        }
    | NotifyMeConfirmed
    | UnsubscribeEmailConfirmed
