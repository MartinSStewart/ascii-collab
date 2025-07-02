module Evergreen.V81.Types exposing (..)

import Browser
import Browser.Navigation
import Bytes
import Dict
import Duration
import Evergreen.V81.Bounds
import Evergreen.V81.Change
import Evergreen.V81.Cursor
import Evergreen.V81.EmailAddress2
import Evergreen.V81.Grid
import Evergreen.V81.Helper
import Evergreen.V81.LocalGrid
import Evergreen.V81.LocalModel
import Evergreen.V81.NotifyMe
import Evergreen.V81.Point2d
import Evergreen.V81.Postmark
import Evergreen.V81.RecentChanges
import Evergreen.V81.Units
import Evergreen.V81.UrlHelper
import Evergreen.V81.User
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
    , windowSize : Evergreen.V81.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V81.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V81.Helper.Coord Evergreen.V81.Units.AsciiUnit
    , mousePosition : Evergreen.V81.Point2d.Point2d Pixels.Pixels Evergreen.V81.Units.ScreenCoordinate
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V81.NotifyMe.Model
    }


type MouseButtonState
    = MouseButtonUp
        { current : Evergreen.V81.Point2d.Point2d Pixels.Pixels Evergreen.V81.Units.ScreenCoordinate
        }
    | MouseButtonDown
        { start : Evergreen.V81.Point2d.Point2d Pixels.Pixels Evergreen.V81.Units.ScreenCoordinate
        , start_ : Evergreen.V81.Point2d.Point2d Evergreen.V81.Units.WorldPixel Evergreen.V81.Units.WorldCoordinate
        , current : Evergreen.V81.Point2d.Point2d Pixels.Pixels Evergreen.V81.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe ( Evergreen.V81.User.UserId, Evergreen.V81.Helper.Coord Evergreen.V81.Units.AsciiUnit ))


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V81.LocalModel.LocalModel Evergreen.V81.Change.Change Evergreen.V81.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V81.Helper.RawCellCoord (WebGL.Mesh Evergreen.V81.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V81.Point2d.Point2d Evergreen.V81.Units.WorldPixel Evergreen.V81.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V81.Point2d.Point2d Evergreen.V81.Units.WorldPixel Evergreen.V81.Units.WorldCoordinate
    , cursor : Evergreen.V81.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V81.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V81.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : Maybe ( Time.Posix, Evergreen.V81.Point2d.Point2d Pixels.Pixels Evergreen.V81.Units.ScreenCoordinate )
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V81.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userHoverHighlighted : Maybe Evergreen.V81.User.UserId
    , highlightContextMenu :
        Maybe
            { userId : Evergreen.V81.User.UserId
            , hidePoint : Evergreen.V81.Helper.Coord Evergreen.V81.Units.AsciiUnit
            }
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V81.NotifyMe.Model
    , textAreaText : String
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData =
    { hiddenUsers : SeqSet.SeqSet Evergreen.V81.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V81.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V81.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V81.Helper.RawCellCoord Int
    }


type alias SubscribedEmail =
    { email : Evergreen.V81.EmailAddress2.EmailAddress
    , frequency : Evergreen.V81.NotifyMe.Frequency
    , confirmTime : Time.Posix
    , userId : Evergreen.V81.User.UserId
    , unsubscribeKey : Evergreen.V81.UrlHelper.UnsubscribeEmailKey
    }


type alias PendingEmail =
    { email : Evergreen.V81.EmailAddress2.EmailAddress
    , frequency : Evergreen.V81.NotifyMe.Frequency
    , creationTime : Time.Posix
    , userId : Evergreen.V81.User.UserId
    , key : Evergreen.V81.UrlHelper.ConfirmEmailKey
    }


type BackendError
    = PostmarkError Evergreen.V81.EmailAddress2.EmailAddress Evergreen.V81.Postmark.SendEmailError


type alias BackendModel =
    { grid : Evergreen.V81.Grid.Grid
    , userSessions :
        Dict.Dict
            Lamdera.SessionId
            { clientIds : Dict.Dict Lamdera.ClientId (Evergreen.V81.Bounds.Bounds Evergreen.V81.Units.CellUnit)
            , userId : Evergreen.V81.User.UserId
            }
    , users : Dict.Dict Evergreen.V81.User.RawUserId BackendUserData
    , usersHiddenRecently :
        List
            { reporter : Evergreen.V81.User.UserId
            , hiddenUser : Evergreen.V81.User.UserId
            , hidePoint : Evergreen.V81.Helper.Coord Evergreen.V81.Units.AsciiUnit
            }
    , userChangesRecently : Evergreen.V81.RecentChanges.RecentChanges
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
    | WindowResized (Evergreen.V81.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V81.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | TextAreaFocused
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V81.Point2d.Point2d Pixels.Pixels Evergreen.V81.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V81.Point2d.Point2d Pixels.Pixels Evergreen.V81.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V81.Point2d.Point2d Pixels.Pixels Evergreen.V81.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V81.Point2d.Point2d Pixels.Pixels Evergreen.V81.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V81.User.UserId
    | UserTagMouseEntered Evergreen.V81.User.UserId
    | UserTagMouseExited Evergreen.V81.User.UserId
    | HideForAllTogglePressed Evergreen.V81.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed
        { userId : Evergreen.V81.User.UserId
        , hidePoint : Evergreen.V81.Helper.Coord Evergreen.V81.Units.AsciiUnit
        }
    | AnimationFrame Time.Posix
    | PressedCancelNotifyMe
    | PressedSubmitNotifyMe Evergreen.V81.NotifyMe.Validated
    | NotifyMeModelChanged Evergreen.V81.NotifyMe.Model
    | ExportBackendPressed
    | ImportBackendPressed
    | FileSelected File.File
    | FileLoaded Bytes.Bytes


type EmailEvent
    = ConfirmationEmailConfirmed_ Evergreen.V81.UrlHelper.ConfirmEmailKey
    | UnsubscribeEmail Evergreen.V81.UrlHelper.UnsubscribeEmailKey


type ToBackend
    = ConnectToBackend (Evergreen.V81.Bounds.Bounds Evergreen.V81.Units.CellUnit) (Maybe EmailEvent)
    | GridChange (List.Nonempty.Nonempty Evergreen.V81.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V81.Bounds.Bounds Evergreen.V81.Units.CellUnit)
    | NotifyMeSubmitted Evergreen.V81.NotifyMe.Validated
    | ExportBackend
    | ImportBackend Bytes.Bytes


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent
    | ConfirmationEmailSent Lamdera.SessionId Time.Posix (Result Evergreen.V81.Postmark.SendEmailError ())
    | ChangeEmailSent Time.Posix Evergreen.V81.EmailAddress2.EmailAddress (Result Evergreen.V81.Postmark.SendEmailError ())
    | UpdateFromFrontend Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix


type alias LoadingData_ =
    { user : Evergreen.V81.User.UserId
    , grid : Evergreen.V81.Grid.Grid
    , hiddenUsers : SeqSet.SeqSet Evergreen.V81.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V81.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V81.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V81.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V81.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V81.Bounds.Bounds Evergreen.V81.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V81.Change.Change)
    | NotifyMeEmailSent
        { isSuccessful : Bool
        }
    | NotifyMeConfirmed
    | UnsubscribeEmailConfirmed
    | BackendExported Bytes.Bytes
    | BackendImported (Result () ())
