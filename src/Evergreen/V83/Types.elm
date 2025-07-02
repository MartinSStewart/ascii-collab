module Evergreen.V83.Types exposing (..)

import Browser
import Browser.Navigation
import Bytes
import Dict
import Duration
import Evergreen.V83.Bounds
import Evergreen.V83.Change
import Evergreen.V83.Cursor
import Evergreen.V83.EmailAddress2
import Evergreen.V83.Grid
import Evergreen.V83.Helper
import Evergreen.V83.LocalGrid
import Evergreen.V83.LocalModel
import Evergreen.V83.NotifyMe
import Evergreen.V83.Point2d
import Evergreen.V83.Postmark
import Evergreen.V83.RecentChanges
import Evergreen.V83.Units
import Evergreen.V83.UrlHelper
import Evergreen.V83.User
import File
import Html.Events.Extra.Mouse
import Keyboard
import Lamdera
import List.Nonempty
import Math.Vector2
import Pixels
import Quantity
import SeqSet
import Time
import Url
import WebGL
import WebGL.Texture


type alias FrontendLoading =
    { key : Browser.Navigation.Key
    , windowSize : Evergreen.V83.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V83.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V83.Helper.Coord Evergreen.V83.Units.AsciiUnit
    , mousePosition : Evergreen.V83.Point2d.Point2d Pixels.Pixels Evergreen.V83.Units.ScreenCoordinate
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V83.NotifyMe.Model
    }


type MouseButtonState
    = MouseButtonUp
        { current : Evergreen.V83.Point2d.Point2d Pixels.Pixels Evergreen.V83.Units.ScreenCoordinate
        }
    | MouseButtonDown
        { start : Evergreen.V83.Point2d.Point2d Pixels.Pixels Evergreen.V83.Units.ScreenCoordinate
        , start_ : Evergreen.V83.Point2d.Point2d Evergreen.V83.Units.WorldPixel Evergreen.V83.Units.WorldCoordinate
        , current : Evergreen.V83.Point2d.Point2d Pixels.Pixels Evergreen.V83.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe ( Evergreen.V83.User.UserId, Evergreen.V83.Helper.Coord Evergreen.V83.Units.AsciiUnit ))


type BackendImported
    = NotImported
    | Importing
    | ImportedSuccessfully
    | ImportFailed


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V83.LocalModel.LocalModel Evergreen.V83.Change.Change Evergreen.V83.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V83.Helper.RawCellCoord (WebGL.Mesh Evergreen.V83.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V83.Point2d.Point2d Evergreen.V83.Units.WorldPixel Evergreen.V83.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V83.Point2d.Point2d Evergreen.V83.Units.WorldPixel Evergreen.V83.Units.WorldCoordinate
    , cursor : Evergreen.V83.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V83.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V83.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : Maybe ( Time.Posix, Evergreen.V83.Point2d.Point2d Pixels.Pixels Evergreen.V83.Units.ScreenCoordinate )
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V83.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userHoverHighlighted : Maybe Evergreen.V83.User.UserId
    , highlightContextMenu :
        Maybe
            { userId : Evergreen.V83.User.UserId
            , hidePoint : Evergreen.V83.Helper.Coord Evergreen.V83.Units.AsciiUnit
            }
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V83.NotifyMe.Model
    , textAreaText : String
    , backendImported : BackendImported
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData =
    { hiddenUsers : SeqSet.SeqSet Evergreen.V83.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V83.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V83.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V83.Helper.RawCellCoord Int
    }


type alias SubscribedEmail =
    { email : Evergreen.V83.EmailAddress2.EmailAddress
    , frequency : Evergreen.V83.NotifyMe.Frequency
    , confirmTime : Time.Posix
    , userId : Evergreen.V83.User.UserId
    , unsubscribeKey : Evergreen.V83.UrlHelper.UnsubscribeEmailKey
    }


type alias PendingEmail =
    { email : Evergreen.V83.EmailAddress2.EmailAddress
    , frequency : Evergreen.V83.NotifyMe.Frequency
    , creationTime : Time.Posix
    , userId : Evergreen.V83.User.UserId
    , key : Evergreen.V83.UrlHelper.ConfirmEmailKey
    }


type BackendError
    = PostmarkError Evergreen.V83.EmailAddress2.EmailAddress Evergreen.V83.Postmark.SendEmailError


type alias BackendModel =
    { grid : Evergreen.V83.Grid.Grid
    , userSessions :
        Dict.Dict
            Lamdera.SessionId
            { clientIds : Dict.Dict Lamdera.ClientId (Evergreen.V83.Bounds.Bounds Evergreen.V83.Units.CellUnit)
            , userId : Evergreen.V83.User.UserId
            }
    , users : Dict.Dict Evergreen.V83.User.RawUserId BackendUserData
    , usersHiddenRecently :
        List
            { reporter : Evergreen.V83.User.UserId
            , hiddenUser : Evergreen.V83.User.UserId
            , hidePoint : Evergreen.V83.Helper.Coord Evergreen.V83.Units.AsciiUnit
            }
    , userChangesRecently : Evergreen.V83.RecentChanges.RecentChanges
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
    | WindowResized (Evergreen.V83.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V83.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | TextAreaFocused
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V83.Point2d.Point2d Pixels.Pixels Evergreen.V83.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V83.Point2d.Point2d Pixels.Pixels Evergreen.V83.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V83.Point2d.Point2d Pixels.Pixels Evergreen.V83.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V83.Point2d.Point2d Pixels.Pixels Evergreen.V83.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V83.User.UserId
    | UserTagMouseEntered Evergreen.V83.User.UserId
    | UserTagMouseExited Evergreen.V83.User.UserId
    | HideForAllTogglePressed Evergreen.V83.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed
        { userId : Evergreen.V83.User.UserId
        , hidePoint : Evergreen.V83.Helper.Coord Evergreen.V83.Units.AsciiUnit
        }
    | AnimationFrame Time.Posix
    | PressedCancelNotifyMe
    | PressedSubmitNotifyMe Evergreen.V83.NotifyMe.Validated
    | NotifyMeModelChanged Evergreen.V83.NotifyMe.Model
    | ExportBackendPressed
    | ImportBackendPressed
    | FileSelected File.File
    | FileLoaded Bytes.Bytes


type EmailEvent
    = ConfirmationEmailConfirmed_ Evergreen.V83.UrlHelper.ConfirmEmailKey
    | UnsubscribeEmail Evergreen.V83.UrlHelper.UnsubscribeEmailKey


type ToBackend
    = ConnectToBackend (Evergreen.V83.Bounds.Bounds Evergreen.V83.Units.CellUnit) (Maybe EmailEvent)
    | GridChange (List.Nonempty.Nonempty Evergreen.V83.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V83.Bounds.Bounds Evergreen.V83.Units.CellUnit)
    | NotifyMeSubmitted Evergreen.V83.NotifyMe.Validated
    | ExportBackend
    | ImportBackend Bytes.Bytes


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent
    | ConfirmationEmailSent Lamdera.SessionId Time.Posix (Result Evergreen.V83.Postmark.SendEmailError ())
    | ChangeEmailSent Time.Posix Evergreen.V83.EmailAddress2.EmailAddress (Result Evergreen.V83.Postmark.SendEmailError ())
    | UpdateFromFrontend Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix


type alias LoadingData_ =
    { user : Evergreen.V83.User.UserId
    , grid : Evergreen.V83.Grid.Grid
    , hiddenUsers : SeqSet.SeqSet Evergreen.V83.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V83.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V83.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V83.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V83.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V83.Bounds.Bounds Evergreen.V83.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V83.Change.Change)
    | NotifyMeEmailSent
        { isSuccessful : Bool
        }
    | NotifyMeConfirmed
    | UnsubscribeEmailConfirmed
    | BackendExported Bytes.Bytes
    | BackendImported (Result () ())
