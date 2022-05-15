module Evergreen.V78.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Duration
import EmailAddress
import Evergreen.V78.Bounds
import Evergreen.V78.Change
import Evergreen.V78.Cursor
import Evergreen.V78.Grid
import Evergreen.V78.Helper
import Evergreen.V78.LocalGrid
import Evergreen.V78.LocalModel
import Evergreen.V78.NotifyMe
import Evergreen.V78.Point2d
import Evergreen.V78.RecentChanges
import Evergreen.V78.Units
import Evergreen.V78.UrlHelper
import Evergreen.V78.User
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
    , windowSize : Evergreen.V78.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V78.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V78.Helper.Coord Evergreen.V78.Units.AsciiUnit
    , mousePosition : Evergreen.V78.Point2d.Point2d Pixels.Pixels Evergreen.V78.Units.ScreenCoordinate
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V78.NotifyMe.Model
    }


type MouseButtonState
    = MouseButtonUp
        { current : Evergreen.V78.Point2d.Point2d Pixels.Pixels Evergreen.V78.Units.ScreenCoordinate
        }
    | MouseButtonDown
        { start : Evergreen.V78.Point2d.Point2d Pixels.Pixels Evergreen.V78.Units.ScreenCoordinate
        , start_ : Evergreen.V78.Point2d.Point2d Evergreen.V78.Units.WorldPixel Evergreen.V78.Units.WorldCoordinate
        , current : Evergreen.V78.Point2d.Point2d Pixels.Pixels Evergreen.V78.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe ( Evergreen.V78.User.UserId, Evergreen.V78.Helper.Coord Evergreen.V78.Units.AsciiUnit ))


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V78.LocalModel.LocalModel Evergreen.V78.Change.Change Evergreen.V78.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V78.Helper.RawCellCoord (WebGL.Mesh Evergreen.V78.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V78.Point2d.Point2d Evergreen.V78.Units.WorldPixel Evergreen.V78.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V78.Point2d.Point2d Evergreen.V78.Units.WorldPixel Evergreen.V78.Units.WorldCoordinate
    , cursor : Evergreen.V78.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V78.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V78.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : Maybe ( Time.Posix, Evergreen.V78.Point2d.Point2d Pixels.Pixels Evergreen.V78.Units.ScreenCoordinate )
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V78.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userHoverHighlighted : Maybe Evergreen.V78.User.UserId
    , highlightContextMenu :
        Maybe
            { userId : Evergreen.V78.User.UserId
            , hidePoint : Evergreen.V78.Helper.Coord Evergreen.V78.Units.AsciiUnit
            }
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V78.NotifyMe.Model
    , textAreaText : String
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData =
    { hiddenUsers : EverySet.EverySet Evergreen.V78.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V78.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V78.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V78.Helper.RawCellCoord Int
    }


type alias SubscribedEmail =
    { email : EmailAddress.EmailAddress
    , frequency : Evergreen.V78.NotifyMe.Frequency
    , confirmTime : Time.Posix
    , userId : Evergreen.V78.User.UserId
    , unsubscribeKey : Evergreen.V78.UrlHelper.UnsubscribeEmailKey
    }


type alias PendingEmail =
    { email : EmailAddress.EmailAddress
    , frequency : Evergreen.V78.NotifyMe.Frequency
    , creationTime : Time.Posix
    , userId : Evergreen.V78.User.UserId
    , key : Evergreen.V78.UrlHelper.ConfirmEmailKey
    }


type BackendError
    = SendGridError EmailAddress.EmailAddress SendGrid.Error


type alias BackendModel =
    { grid : Evergreen.V78.Grid.Grid
    , userSessions :
        Dict.Dict
            Lamdera.SessionId
            { clientIds : Dict.Dict Lamdera.ClientId (Evergreen.V78.Bounds.Bounds Evergreen.V78.Units.CellUnit)
            , userId : Evergreen.V78.User.UserId
            }
    , users : Dict.Dict Evergreen.V78.User.RawUserId BackendUserData
    , usersHiddenRecently :
        List
            { reporter : Evergreen.V78.User.UserId
            , hiddenUser : Evergreen.V78.User.UserId
            , hidePoint : Evergreen.V78.Helper.Coord Evergreen.V78.Units.AsciiUnit
            }
    , userChangesRecently : Evergreen.V78.RecentChanges.RecentChanges
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
    | WindowResized (Evergreen.V78.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V78.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | TextAreaFocused
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V78.Point2d.Point2d Pixels.Pixels Evergreen.V78.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V78.Point2d.Point2d Pixels.Pixels Evergreen.V78.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V78.Point2d.Point2d Pixels.Pixels Evergreen.V78.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V78.Point2d.Point2d Pixels.Pixels Evergreen.V78.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V78.User.UserId
    | UserTagMouseEntered Evergreen.V78.User.UserId
    | UserTagMouseExited Evergreen.V78.User.UserId
    | HideForAllTogglePressed Evergreen.V78.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed
        { userId : Evergreen.V78.User.UserId
        , hidePoint : Evergreen.V78.Helper.Coord Evergreen.V78.Units.AsciiUnit
        }
    | AnimationFrame Time.Posix
    | PressedCancelNotifyMe
    | PressedSubmitNotifyMe Evergreen.V78.NotifyMe.Validated
    | NotifyMeModelChanged Evergreen.V78.NotifyMe.Model


type EmailEvent
    = ConfirmationEmailConfirmed_ Evergreen.V78.UrlHelper.ConfirmEmailKey
    | UnsubscribeEmail Evergreen.V78.UrlHelper.UnsubscribeEmailKey


type ToBackend
    = ConnectToBackend (Evergreen.V78.Bounds.Bounds Evergreen.V78.Units.CellUnit) (Maybe EmailEvent)
    | GridChange (List.Nonempty.Nonempty Evergreen.V78.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V78.Bounds.Bounds Evergreen.V78.Units.CellUnit)
    | NotifyMeSubmitted Evergreen.V78.NotifyMe.Validated


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent
    | ConfirmationEmailSent Lamdera.SessionId Time.Posix (Result SendGrid.Error ())
    | ChangeEmailSent Time.Posix EmailAddress.EmailAddress (Result SendGrid.Error ())
    | UpdateFromFrontend Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix


type alias LoadingData_ =
    { user : Evergreen.V78.User.UserId
    , grid : Evergreen.V78.Grid.Grid
    , hiddenUsers : EverySet.EverySet Evergreen.V78.User.UserId
    , adminHiddenUsers : EverySet.EverySet Evergreen.V78.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V78.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V78.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V78.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V78.Bounds.Bounds Evergreen.V78.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V78.Change.Change)
    | NotifyMeEmailSent
        { isSuccessful : Bool
        }
    | NotifyMeConfirmed
    | UnsubscribeEmailConfirmed
