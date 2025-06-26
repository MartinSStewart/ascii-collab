module Evergreen.V73.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Duration
import EmailAddress
import Evergreen.V73.Bounds
import Evergreen.V73.Change
import Evergreen.V73.Cursor
import Evergreen.V73.Grid
import Evergreen.V73.Helper
import Evergreen.V73.LocalGrid
import Evergreen.V73.LocalModel
import Evergreen.V73.NotifyMe
import Evergreen.V73.Point2d
import Evergreen.V73.RecentChanges
import Evergreen.V73.Units
import Evergreen.V73.UrlHelper
import Evergreen.V73.User
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
    , windowSize : Evergreen.V73.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V73.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V73.Helper.Coord Evergreen.V73.Units.AsciiUnit
    , mousePosition : Evergreen.V73.Point2d.Point2d Pixels.Pixels Evergreen.V73.Units.ScreenCoordinate
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V73.NotifyMe.Model
    }


type MouseButtonState
    = MouseButtonUp
        { current : Evergreen.V73.Point2d.Point2d Pixels.Pixels Evergreen.V73.Units.ScreenCoordinate
        }
    | MouseButtonDown
        { start : Evergreen.V73.Point2d.Point2d Pixels.Pixels Evergreen.V73.Units.ScreenCoordinate
        , start_ : Evergreen.V73.Point2d.Point2d Evergreen.V73.Units.WorldPixel Evergreen.V73.Units.WorldCoordinate
        , current : Evergreen.V73.Point2d.Point2d Pixels.Pixels Evergreen.V73.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe ( Evergreen.V73.User.UserId, Evergreen.V73.Helper.Coord Evergreen.V73.Units.AsciiUnit ))


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V73.LocalModel.LocalModel Evergreen.V73.Change.Change Evergreen.V73.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V73.Helper.RawCellCoord (WebGL.Mesh Evergreen.V73.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V73.Point2d.Point2d Evergreen.V73.Units.WorldPixel Evergreen.V73.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V73.Point2d.Point2d Evergreen.V73.Units.WorldPixel Evergreen.V73.Units.WorldCoordinate
    , cursor : Evergreen.V73.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V73.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V73.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : Maybe ( Time.Posix, Evergreen.V73.Point2d.Point2d Pixels.Pixels Evergreen.V73.Units.ScreenCoordinate )
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V73.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userHoverHighlighted : Maybe Evergreen.V73.User.UserId
    , highlightContextMenu :
        Maybe
            { userId : Evergreen.V73.User.UserId
            , hidePoint : Evergreen.V73.Helper.Coord Evergreen.V73.Units.AsciiUnit
            }
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V73.NotifyMe.Model
    , textAreaText : String
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData =
    { hiddenUsers : SeqSet.SeqSet Evergreen.V73.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V73.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V73.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V73.Helper.RawCellCoord Int
    }


type alias SubscribedEmail =
    { email : EmailAddress.EmailAddress
    , frequency : Evergreen.V73.NotifyMe.Frequency
    , confirmTime : Time.Posix
    , userId : Evergreen.V73.User.UserId
    , unsubscribeKey : Evergreen.V73.UrlHelper.UnsubscribeEmailKey
    }


type alias PendingEmail =
    { email : EmailAddress.EmailAddress
    , frequency : Evergreen.V73.NotifyMe.Frequency
    , creationTime : Time.Posix
    , userId : Evergreen.V73.User.UserId
    , key : Evergreen.V73.UrlHelper.ConfirmEmailKey
    }


type BackendError
    = SendGridError EmailAddress.EmailAddress SendGrid.Error


type alias BackendModel =
    { grid : Evergreen.V73.Grid.Grid
    , userSessions :
        Dict.Dict
            Lamdera.SessionId
            { clientIds : Dict.Dict Lamdera.ClientId (Evergreen.V73.Bounds.Bounds Evergreen.V73.Units.CellUnit)
            , userId : Evergreen.V73.User.UserId
            }
    , users : Dict.Dict Evergreen.V73.User.RawUserId BackendUserData
    , usersHiddenRecently :
        List
            { reporter : Evergreen.V73.User.UserId
            , hiddenUser : Evergreen.V73.User.UserId
            , hidePoint : Evergreen.V73.Helper.Coord Evergreen.V73.Units.AsciiUnit
            }
    , userChangesRecently : Evergreen.V73.RecentChanges.RecentChanges
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
    | WindowResized (Evergreen.V73.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V73.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | TextAreaFocused
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V73.Point2d.Point2d Pixels.Pixels Evergreen.V73.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V73.Point2d.Point2d Pixels.Pixels Evergreen.V73.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V73.Point2d.Point2d Pixels.Pixels Evergreen.V73.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V73.Point2d.Point2d Pixels.Pixels Evergreen.V73.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V73.User.UserId
    | UserTagMouseEntered Evergreen.V73.User.UserId
    | UserTagMouseExited Evergreen.V73.User.UserId
    | HideForAllTogglePressed Evergreen.V73.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed
        { userId : Evergreen.V73.User.UserId
        , hidePoint : Evergreen.V73.Helper.Coord Evergreen.V73.Units.AsciiUnit
        }
    | AnimationFrame Time.Posix
    | PressedCancelNotifyMe
    | PressedSubmitNotifyMe Evergreen.V73.NotifyMe.Validated
    | NotifyMeModelChanged Evergreen.V73.NotifyMe.Model


type EmailEvent
    = ConfirmationEmailConfirmed_ Evergreen.V73.UrlHelper.ConfirmEmailKey
    | UnsubscribeEmail Evergreen.V73.UrlHelper.UnsubscribeEmailKey


type ToBackend
    = ConnectToBackend (Evergreen.V73.Bounds.Bounds Evergreen.V73.Units.CellUnit) (Maybe EmailEvent)
    | GridChange (List.Nonempty.Nonempty Evergreen.V73.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V73.Bounds.Bounds Evergreen.V73.Units.CellUnit)
    | NotifyMeSubmitted Evergreen.V73.NotifyMe.Validated


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent
    | ConfirmationEmailSent Lamdera.SessionId Time.Posix (Result SendGrid.Error ())
    | ChangeEmailSent Time.Posix EmailAddress.EmailAddress (Result SendGrid.Error ())
    | UpdateFromFrontend Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix


type alias LoadingData_ =
    { user : Evergreen.V73.User.UserId
    , grid : Evergreen.V73.Grid.Grid
    , hiddenUsers : SeqSet.SeqSet Evergreen.V73.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V73.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V73.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V73.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V73.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V73.Bounds.Bounds Evergreen.V73.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V73.Change.Change)
    | NotifyMeEmailSent
        { isSuccessful : Bool
        }
    | NotifyMeConfirmed
    | UnsubscribeEmailConfirmed
