module Evergreen.V80.Types exposing (..)

import Browser
import Browser.Navigation
import Bytes
import Dict
import Duration
import EmailAddress
import Evergreen.V80.Bounds
import Evergreen.V80.Change
import Evergreen.V80.Cursor
import Evergreen.V80.Grid
import Evergreen.V80.Helper
import Evergreen.V80.LocalGrid
import Evergreen.V80.LocalModel
import Evergreen.V80.NotifyMe
import Evergreen.V80.Point2d
import Evergreen.V80.RecentChanges
import Evergreen.V80.Units
import Evergreen.V80.UrlHelper
import Evergreen.V80.User
import EverySet
import File
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
    , windowSize : Evergreen.V80.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V80.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V80.Helper.Coord Evergreen.V80.Units.AsciiUnit
    , mousePosition : Evergreen.V80.Point2d.Point2d Pixels.Pixels Evergreen.V80.Units.ScreenCoordinate
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V80.NotifyMe.Model
    }


type MouseButtonState
    = MouseButtonUp
        { current : Evergreen.V80.Point2d.Point2d Pixels.Pixels Evergreen.V80.Units.ScreenCoordinate
        }
    | MouseButtonDown
        { start : Evergreen.V80.Point2d.Point2d Pixels.Pixels Evergreen.V80.Units.ScreenCoordinate
        , start_ : Evergreen.V80.Point2d.Point2d Evergreen.V80.Units.WorldPixel Evergreen.V80.Units.WorldCoordinate
        , current : Evergreen.V80.Point2d.Point2d Pixels.Pixels Evergreen.V80.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe ( Evergreen.V80.User.UserId, Evergreen.V80.Helper.Coord Evergreen.V80.Units.AsciiUnit ))


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V80.LocalModel.LocalModel Evergreen.V80.Change.Change Evergreen.V80.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V80.Helper.RawCellCoord (WebGL.Mesh Evergreen.V80.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V80.Point2d.Point2d Evergreen.V80.Units.WorldPixel Evergreen.V80.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V80.Point2d.Point2d Evergreen.V80.Units.WorldPixel Evergreen.V80.Units.WorldCoordinate
    , cursor : Evergreen.V80.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V80.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V80.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : Maybe ( Time.Posix, Evergreen.V80.Point2d.Point2d Pixels.Pixels Evergreen.V80.Units.ScreenCoordinate )
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V80.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userHoverHighlighted : Maybe Evergreen.V80.User.UserId
    , highlightContextMenu :
        Maybe
            { userId : Evergreen.V80.User.UserId
            , hidePoint : Evergreen.V80.Helper.Coord Evergreen.V80.Units.AsciiUnit
            }
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V80.NotifyMe.Model
    , textAreaText : String
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData =
    { hiddenUsers : EverySet.EverySet Evergreen.V80.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V80.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V80.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V80.Helper.RawCellCoord Int
    }


type alias SubscribedEmail =
    { email : EmailAddress.EmailAddress
    , frequency : Evergreen.V80.NotifyMe.Frequency
    , confirmTime : Time.Posix
    , userId : Evergreen.V80.User.UserId
    , unsubscribeKey : Evergreen.V80.UrlHelper.UnsubscribeEmailKey
    }


type alias PendingEmail =
    { email : EmailAddress.EmailAddress
    , frequency : Evergreen.V80.NotifyMe.Frequency
    , creationTime : Time.Posix
    , userId : Evergreen.V80.User.UserId
    , key : Evergreen.V80.UrlHelper.ConfirmEmailKey
    }


type BackendError
    = SendGridError EmailAddress.EmailAddress SendGrid.Error


type alias BackendModel =
    { grid : Evergreen.V80.Grid.Grid
    , userSessions :
        Dict.Dict
            Lamdera.SessionId
            { clientIds : Dict.Dict Lamdera.ClientId (Evergreen.V80.Bounds.Bounds Evergreen.V80.Units.CellUnit)
            , userId : Evergreen.V80.User.UserId
            }
    , users : Dict.Dict Evergreen.V80.User.RawUserId BackendUserData
    , usersHiddenRecently :
        List
            { reporter : Evergreen.V80.User.UserId
            , hiddenUser : Evergreen.V80.User.UserId
            , hidePoint : Evergreen.V80.Helper.Coord Evergreen.V80.Units.AsciiUnit
            }
    , userChangesRecently : Evergreen.V80.RecentChanges.RecentChanges
    , subscribedEmails : List SubscribedEmail
    , pendingEmails : List PendingEmail
    , secretLinkCounter : Int
    , errors : List ( Time.Posix, BackendError )
    , dummyField : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextureLoaded (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Evergreen.V80.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V80.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | TextAreaFocused
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V80.Point2d.Point2d Pixels.Pixels Evergreen.V80.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V80.Point2d.Point2d Pixels.Pixels Evergreen.V80.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V80.Point2d.Point2d Pixels.Pixels Evergreen.V80.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V80.Point2d.Point2d Pixels.Pixels Evergreen.V80.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V80.User.UserId
    | UserTagMouseEntered Evergreen.V80.User.UserId
    | UserTagMouseExited Evergreen.V80.User.UserId
    | HideForAllTogglePressed Evergreen.V80.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed
        { userId : Evergreen.V80.User.UserId
        , hidePoint : Evergreen.V80.Helper.Coord Evergreen.V80.Units.AsciiUnit
        }
    | AnimationFrame Time.Posix
    | PressedCancelNotifyMe
    | PressedSubmitNotifyMe Evergreen.V80.NotifyMe.Validated
    | NotifyMeModelChanged Evergreen.V80.NotifyMe.Model
    | ExportBackendPressed
    | ImportBackendPressed
    | FileSelected File.File
    | FileLoaded Bytes.Bytes


type EmailEvent
    = ConfirmationEmailConfirmed_ Evergreen.V80.UrlHelper.ConfirmEmailKey
    | UnsubscribeEmail Evergreen.V80.UrlHelper.UnsubscribeEmailKey


type ToBackend
    = ConnectToBackend (Evergreen.V80.Bounds.Bounds Evergreen.V80.Units.CellUnit) (Maybe EmailEvent)
    | GridChange (List.Nonempty.Nonempty Evergreen.V80.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V80.Bounds.Bounds Evergreen.V80.Units.CellUnit)
    | NotifyMeSubmitted Evergreen.V80.NotifyMe.Validated
    | ExportBackend
    | ImportBackend Bytes.Bytes


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent
    | ConfirmationEmailSent Lamdera.SessionId Time.Posix (Result SendGrid.Error ())
    | ChangeEmailSent Time.Posix EmailAddress.EmailAddress (Result SendGrid.Error ())
    | UpdateFromFrontend Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix


type alias LoadingData_ =
    { user : Evergreen.V80.User.UserId
    , grid : Evergreen.V80.Grid.Grid
    , hiddenUsers : EverySet.EverySet Evergreen.V80.User.UserId
    , adminHiddenUsers : EverySet.EverySet Evergreen.V80.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V80.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V80.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V80.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V80.Bounds.Bounds Evergreen.V80.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V80.Change.Change)
    | NotifyMeEmailSent
        { isSuccessful : Bool
        }
    | NotifyMeConfirmed
    | UnsubscribeEmailConfirmed
    | BackendExported Bytes.Bytes
    | BackendImported (Result () ())
