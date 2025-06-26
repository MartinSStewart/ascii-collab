module Evergreen.V79.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Duration
import EmailAddress
import Evergreen.V79.Bounds
import Evergreen.V79.Change
import Evergreen.V79.Cursor
import Evergreen.V79.Grid
import Evergreen.V79.Helper
import Evergreen.V79.LocalGrid
import Evergreen.V79.LocalModel
import Evergreen.V79.NotifyMe
import Evergreen.V79.Point2d
import Evergreen.V79.RecentChanges
import Evergreen.V79.Units
import Evergreen.V79.UrlHelper
import Evergreen.V79.User
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
    , windowSize : Evergreen.V79.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V79.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V79.Helper.Coord Evergreen.V79.Units.AsciiUnit
    , mousePosition : Evergreen.V79.Point2d.Point2d Pixels.Pixels Evergreen.V79.Units.ScreenCoordinate
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V79.NotifyMe.Model
    }


type MouseButtonState
    = MouseButtonUp
        { current : Evergreen.V79.Point2d.Point2d Pixels.Pixels Evergreen.V79.Units.ScreenCoordinate
        }
    | MouseButtonDown
        { start : Evergreen.V79.Point2d.Point2d Pixels.Pixels Evergreen.V79.Units.ScreenCoordinate
        , start_ : Evergreen.V79.Point2d.Point2d Evergreen.V79.Units.WorldPixel Evergreen.V79.Units.WorldCoordinate
        , current : Evergreen.V79.Point2d.Point2d Pixels.Pixels Evergreen.V79.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe ( Evergreen.V79.User.UserId, Evergreen.V79.Helper.Coord Evergreen.V79.Units.AsciiUnit ))


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V79.LocalModel.LocalModel Evergreen.V79.Change.Change Evergreen.V79.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V79.Helper.RawCellCoord (WebGL.Mesh Evergreen.V79.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V79.Point2d.Point2d Evergreen.V79.Units.WorldPixel Evergreen.V79.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V79.Point2d.Point2d Evergreen.V79.Units.WorldPixel Evergreen.V79.Units.WorldCoordinate
    , cursor : Evergreen.V79.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V79.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V79.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : Maybe ( Time.Posix, Evergreen.V79.Point2d.Point2d Pixels.Pixels Evergreen.V79.Units.ScreenCoordinate )
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V79.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userHoverHighlighted : Maybe Evergreen.V79.User.UserId
    , highlightContextMenu :
        Maybe
            { userId : Evergreen.V79.User.UserId
            , hidePoint : Evergreen.V79.Helper.Coord Evergreen.V79.Units.AsciiUnit
            }
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V79.NotifyMe.Model
    , textAreaText : String
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData =
    { hiddenUsers : SeqSet.SeqSet Evergreen.V79.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V79.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V79.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V79.Helper.RawCellCoord Int
    }


type alias SubscribedEmail =
    { email : EmailAddress.EmailAddress
    , frequency : Evergreen.V79.NotifyMe.Frequency
    , confirmTime : Time.Posix
    , userId : Evergreen.V79.User.UserId
    , unsubscribeKey : Evergreen.V79.UrlHelper.UnsubscribeEmailKey
    }


type alias PendingEmail =
    { email : EmailAddress.EmailAddress
    , frequency : Evergreen.V79.NotifyMe.Frequency
    , creationTime : Time.Posix
    , userId : Evergreen.V79.User.UserId
    , key : Evergreen.V79.UrlHelper.ConfirmEmailKey
    }


type BackendError
    = SendGridError EmailAddress.EmailAddress SendGrid.Error


type alias BackendModel =
    { grid : Evergreen.V79.Grid.Grid
    , userSessions :
        Dict.Dict
            Lamdera.SessionId
            { clientIds : Dict.Dict Lamdera.ClientId (Evergreen.V79.Bounds.Bounds Evergreen.V79.Units.CellUnit)
            , userId : Evergreen.V79.User.UserId
            }
    , users : Dict.Dict Evergreen.V79.User.RawUserId BackendUserData
    , usersHiddenRecently :
        List
            { reporter : Evergreen.V79.User.UserId
            , hiddenUser : Evergreen.V79.User.UserId
            , hidePoint : Evergreen.V79.Helper.Coord Evergreen.V79.Units.AsciiUnit
            }
    , userChangesRecently : Evergreen.V79.RecentChanges.RecentChanges
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
    | WindowResized (Evergreen.V79.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V79.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | TextAreaFocused
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V79.Point2d.Point2d Pixels.Pixels Evergreen.V79.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V79.Point2d.Point2d Pixels.Pixels Evergreen.V79.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V79.Point2d.Point2d Pixels.Pixels Evergreen.V79.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V79.Point2d.Point2d Pixels.Pixels Evergreen.V79.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V79.User.UserId
    | UserTagMouseEntered Evergreen.V79.User.UserId
    | UserTagMouseExited Evergreen.V79.User.UserId
    | HideForAllTogglePressed Evergreen.V79.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed
        { userId : Evergreen.V79.User.UserId
        , hidePoint : Evergreen.V79.Helper.Coord Evergreen.V79.Units.AsciiUnit
        }
    | AnimationFrame Time.Posix
    | PressedCancelNotifyMe
    | PressedSubmitNotifyMe Evergreen.V79.NotifyMe.Validated
    | NotifyMeModelChanged Evergreen.V79.NotifyMe.Model


type EmailEvent
    = ConfirmationEmailConfirmed_ Evergreen.V79.UrlHelper.ConfirmEmailKey
    | UnsubscribeEmail Evergreen.V79.UrlHelper.UnsubscribeEmailKey


type ToBackend
    = ConnectToBackend (Evergreen.V79.Bounds.Bounds Evergreen.V79.Units.CellUnit) (Maybe EmailEvent)
    | GridChange (List.Nonempty.Nonempty Evergreen.V79.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V79.Bounds.Bounds Evergreen.V79.Units.CellUnit)
    | NotifyMeSubmitted Evergreen.V79.NotifyMe.Validated


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent
    | ConfirmationEmailSent Lamdera.SessionId Time.Posix (Result SendGrid.Error ())
    | ChangeEmailSent Time.Posix EmailAddress.EmailAddress (Result SendGrid.Error ())
    | UpdateFromFrontend Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix


type alias LoadingData_ =
    { user : Evergreen.V79.User.UserId
    , grid : Evergreen.V79.Grid.Grid
    , hiddenUsers : SeqSet.SeqSet Evergreen.V79.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V79.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V79.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V79.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V79.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V79.Bounds.Bounds Evergreen.V79.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V79.Change.Change)
    | NotifyMeEmailSent
        { isSuccessful : Bool
        }
    | NotifyMeConfirmed
    | UnsubscribeEmailConfirmed
