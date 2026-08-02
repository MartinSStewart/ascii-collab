module Evergreen.V85.Types exposing (..)

import Browser
import Browser.Navigation
import Bytes
import Dict
import Duration
import EmailAddress
import Evergreen.V85.Bounds
import Evergreen.V85.Change
import Evergreen.V85.Cursor
import Evergreen.V85.Grid
import Evergreen.V85.Helper
import Evergreen.V85.LocalGrid
import Evergreen.V85.LocalModel
import Evergreen.V85.NotifyMe
import Evergreen.V85.Point2d
import Evergreen.V85.RecentChanges
import Evergreen.V85.Units
import Evergreen.V85.UrlHelper
import Evergreen.V85.User
import File
import Html.Events.Extra.Mouse
import Keyboard
import Lamdera
import List.Nonempty
import Math.Vector2
import Pixels
import Postmark
import Quantity
import SeqSet
import Time
import Url
import WebGL
import WebGL.Texture


type alias FrontendLoading =
    { key : Browser.Navigation.Key
    , windowSize : Evergreen.V85.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V85.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V85.Helper.Coord Evergreen.V85.Units.AsciiUnit
    , mousePosition : Evergreen.V85.Point2d.Point2d Pixels.Pixels Evergreen.V85.Units.ScreenCoordinate
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V85.NotifyMe.Model
    }


type MouseButtonState
    = MouseButtonUp
        { current : Evergreen.V85.Point2d.Point2d Pixels.Pixels Evergreen.V85.Units.ScreenCoordinate
        }
    | MouseButtonDown
        { start : Evergreen.V85.Point2d.Point2d Pixels.Pixels Evergreen.V85.Units.ScreenCoordinate
        , start_ : Evergreen.V85.Point2d.Point2d Evergreen.V85.Units.WorldPixel Evergreen.V85.Units.WorldCoordinate
        , current : Evergreen.V85.Point2d.Point2d Pixels.Pixels Evergreen.V85.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe ( Evergreen.V85.User.UserId, Evergreen.V85.Helper.Coord Evergreen.V85.Units.AsciiUnit ))


type BackendImported
    = NotImported
    | Importing
    | ImportedSuccessfully
    | ImportFailed


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V85.LocalModel.LocalModel Evergreen.V85.Change.Change Evergreen.V85.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V85.Helper.RawCellCoord (WebGL.Mesh Evergreen.V85.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V85.Point2d.Point2d Evergreen.V85.Units.WorldPixel Evergreen.V85.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V85.Point2d.Point2d Evergreen.V85.Units.WorldPixel Evergreen.V85.Units.WorldCoordinate
    , cursor : Evergreen.V85.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V85.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V85.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : Maybe ( Time.Posix, Evergreen.V85.Point2d.Point2d Pixels.Pixels Evergreen.V85.Units.ScreenCoordinate )
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V85.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userHoverHighlighted : Maybe Evergreen.V85.User.UserId
    , highlightContextMenu :
        Maybe
            { userId : Evergreen.V85.User.UserId
            , hidePoint : Evergreen.V85.Helper.Coord Evergreen.V85.Units.AsciiUnit
            }
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V85.NotifyMe.Model
    , textAreaText : String
    , backendImported : BackendImported
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData =
    { hiddenUsers : SeqSet.SeqSet Evergreen.V85.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V85.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V85.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V85.Helper.RawCellCoord Int
    }


type alias SubscribedEmail =
    { email : EmailAddress.EmailAddress
    , frequency : Evergreen.V85.NotifyMe.Frequency
    , confirmTime : Time.Posix
    , userId : Evergreen.V85.User.UserId
    , unsubscribeKey : Evergreen.V85.UrlHelper.UnsubscribeEmailKey
    }


type alias PendingEmail =
    { email : EmailAddress.EmailAddress
    , frequency : Evergreen.V85.NotifyMe.Frequency
    , creationTime : Time.Posix
    , userId : Evergreen.V85.User.UserId
    , key : Evergreen.V85.UrlHelper.ConfirmEmailKey
    }


type BackendError
    = PostmarkError EmailAddress.EmailAddress Postmark.SendEmailError


type alias BackendModel =
    { grid : Evergreen.V85.Grid.Grid
    , userSessions :
        Dict.Dict
            Lamdera.SessionId
            { clientIds : Dict.Dict Lamdera.ClientId (Evergreen.V85.Bounds.Bounds Evergreen.V85.Units.CellUnit)
            , userId : Evergreen.V85.User.UserId
            }
    , users : Dict.Dict Evergreen.V85.User.RawUserId BackendUserData
    , usersHiddenRecently :
        List
            { reporter : Evergreen.V85.User.UserId
            , hiddenUser : Evergreen.V85.User.UserId
            , hidePoint : Evergreen.V85.Helper.Coord Evergreen.V85.Units.AsciiUnit
            }
    , userChangesRecently : Evergreen.V85.RecentChanges.RecentChanges
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
    | WindowResized (Evergreen.V85.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V85.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | TextAreaFocused
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V85.Point2d.Point2d Pixels.Pixels Evergreen.V85.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V85.Point2d.Point2d Pixels.Pixels Evergreen.V85.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V85.Point2d.Point2d Pixels.Pixels Evergreen.V85.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V85.Point2d.Point2d Pixels.Pixels Evergreen.V85.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V85.User.UserId
    | UserTagMouseEntered Evergreen.V85.User.UserId
    | UserTagMouseExited Evergreen.V85.User.UserId
    | HideForAllTogglePressed Evergreen.V85.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed
        { userId : Evergreen.V85.User.UserId
        , hidePoint : Evergreen.V85.Helper.Coord Evergreen.V85.Units.AsciiUnit
        }
    | AnimationFrame Time.Posix
    | PressedCancelNotifyMe
    | PressedSubmitNotifyMe Evergreen.V85.NotifyMe.Validated
    | NotifyMeModelChanged Evergreen.V85.NotifyMe.Model
    | ExportBackendPressed
    | ImportBackendPressed
    | FileSelected File.File
    | FileLoaded Bytes.Bytes


type EmailEvent
    = ConfirmationEmailConfirmed_ Evergreen.V85.UrlHelper.ConfirmEmailKey
    | UnsubscribeEmail Evergreen.V85.UrlHelper.UnsubscribeEmailKey


type ToBackend
    = ConnectToBackend (Evergreen.V85.Bounds.Bounds Evergreen.V85.Units.CellUnit) (Maybe EmailEvent)
    | GridChange (List.Nonempty.Nonempty Evergreen.V85.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V85.Bounds.Bounds Evergreen.V85.Units.CellUnit)
    | NotifyMeSubmitted Evergreen.V85.NotifyMe.Validated
    | ExportBackend
    | ImportBackend Bytes.Bytes


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent
    | ConfirmationEmailSent Lamdera.SessionId Time.Posix (Result Postmark.SendEmailError ())
    | ChangeEmailSent Time.Posix EmailAddress.EmailAddress (Result Postmark.SendEmailError ())
    | UpdateFromFrontend Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix


type alias LoadingData_ =
    { user : Evergreen.V85.User.UserId
    , grid : Evergreen.V85.Grid.Grid
    , hiddenUsers : SeqSet.SeqSet Evergreen.V85.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V85.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V85.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V85.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V85.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V85.Bounds.Bounds Evergreen.V85.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V85.Change.Change)
    | NotifyMeEmailSent
        { isSuccessful : Bool
        }
    | NotifyMeConfirmed
    | UnsubscribeEmailConfirmed
    | BackendExported Bytes.Bytes
    | BackendImported (Result () ())
