module Evergreen.V47.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Duration
import Email
import Evergreen.V47.Bounds
import Evergreen.V47.Change
import Evergreen.V47.Cursor
import Evergreen.V47.Grid
import Evergreen.V47.Helper
import Evergreen.V47.LocalGrid
import Evergreen.V47.LocalModel
import Evergreen.V47.NotifyMe
import Evergreen.V47.Point2d
import Evergreen.V47.RecentChanges
import Evergreen.V47.Units
import Evergreen.V47.UrlHelper
import Evergreen.V47.User
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
    , windowSize : Evergreen.V47.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V47.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , time : Time.Posix
    , viewPoint : Evergreen.V47.Helper.Coord Evergreen.V47.Units.AsciiUnit
    , mousePosition : Evergreen.V47.Point2d.Point2d Pixels.Pixels Evergreen.V47.Units.ScreenCoordinate
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V47.NotifyMe.Model
    }


type MouseButtonState
    = MouseButtonUp
        { current : Evergreen.V47.Point2d.Point2d Pixels.Pixels Evergreen.V47.Units.ScreenCoordinate
        }
    | MouseButtonDown
        { start : Evergreen.V47.Point2d.Point2d Pixels.Pixels Evergreen.V47.Units.ScreenCoordinate
        , start_ : Evergreen.V47.Point2d.Point2d Evergreen.V47.Units.WorldPixel Evergreen.V47.Units.WorldCoordinate
        , current : Evergreen.V47.Point2d.Point2d Pixels.Pixels Evergreen.V47.Units.ScreenCoordinate
        }


type ToolType
    = DragTool
    | SelectTool
    | HighlightTool (Maybe ( Evergreen.V47.User.UserId, Evergreen.V47.Helper.Coord Evergreen.V47.Units.AsciiUnit ))


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , localModel : Evergreen.V47.LocalModel.LocalModel Evergreen.V47.Change.Change Evergreen.V47.LocalGrid.LocalGrid
    , meshes : Dict.Dict Evergreen.V47.Helper.RawCellCoord (WebGL.Mesh Evergreen.V47.Grid.Vertex)
    , cursorMesh :
        WebGL.Mesh
            { position : Math.Vector2.Vec2
            }
    , viewPoint : Evergreen.V47.Point2d.Point2d Evergreen.V47.Units.WorldPixel Evergreen.V47.Units.WorldCoordinate
    , viewPointLastInterval : Evergreen.V47.Point2d.Point2d Evergreen.V47.Units.WorldPixel Evergreen.V47.Units.WorldCoordinate
    , cursor : Evergreen.V47.Cursor.Cursor
    , texture : Maybe WebGL.Texture.Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Evergreen.V47.Helper.Coord Pixels.Pixels
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V47.Units.WorldPixel Pixels.Pixels)
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , lastMouseLeftUp : Maybe ( Time.Posix, Evergreen.V47.Point2d.Point2d Pixels.Pixels Evergreen.V47.Units.ScreenCoordinate )
    , mouseMiddle : MouseButtonState
    , pendingChanges : List Evergreen.V47.Change.LocalChange
    , tool : ToolType
    , undoAddLast : Time.Posix
    , time : Time.Posix
    , lastTouchMove : Maybe Time.Posix
    , userHoverHighlighted : Maybe Evergreen.V47.User.UserId
    , highlightContextMenu :
        Maybe
            { userId : Evergreen.V47.User.UserId
            , hidePoint : Evergreen.V47.Helper.Coord Evergreen.V47.Units.AsciiUnit
            }
    , adminEnabled : Bool
    , animationElapsedTime : Duration.Duration
    , ignoreNextUrlChanged : Bool
    , showNotifyMe : Bool
    , notifyMeModel : Evergreen.V47.NotifyMe.Model
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendUserData =
    { hiddenUsers : SeqSet.SeqSet Evergreen.V47.User.UserId
    , hiddenForAll : Bool
    , undoHistory : List (Dict.Dict Evergreen.V47.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V47.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V47.Helper.RawCellCoord Int
    }


type alias SubscribedEmail =
    { email : Email.Email
    , frequency : Evergreen.V47.NotifyMe.Frequency
    , confirmTime : Time.Posix
    , userId : Evergreen.V47.User.UserId
    , unsubscribeKey : Evergreen.V47.UrlHelper.UnsubscribeEmailKey
    }


type alias PendingEmail =
    { email : Email.Email
    , frequency : Evergreen.V47.NotifyMe.Frequency
    , creationTime : Time.Posix
    , userId : Evergreen.V47.User.UserId
    , key : Evergreen.V47.UrlHelper.ConfirmEmailKey
    }


type BackendError
    = SendGridError Email.Email SendGrid.Error


type alias BackendModel =
    { grid : Evergreen.V47.Grid.Grid
    , userSessions :
        Dict.Dict
            Lamdera.SessionId
            { clientIds : Dict.Dict Lamdera.ClientId (Evergreen.V47.Bounds.Bounds Evergreen.V47.Units.CellUnit)
            , userId : Evergreen.V47.User.UserId
            }
    , users : Dict.Dict Evergreen.V47.User.RawUserId BackendUserData
    , usersHiddenRecently :
        List
            { reporter : Evergreen.V47.User.UserId
            , hiddenUser : Evergreen.V47.User.UserId
            , hidePoint : Evergreen.V47.Helper.Coord Evergreen.V47.Units.AsciiUnit
            }
    , userChangesRecently : Evergreen.V47.RecentChanges.RecentChanges
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
    | WindowResized (Evergreen.V47.Helper.Coord Pixels.Pixels)
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V47.Units.WorldPixel Pixels.Pixels))
    | UserTyped String
    | MouseDown Html.Events.Extra.Mouse.Button (Evergreen.V47.Point2d.Point2d Pixels.Pixels Evergreen.V47.Units.ScreenCoordinate)
    | MouseUp Html.Events.Extra.Mouse.Button (Evergreen.V47.Point2d.Point2d Pixels.Pixels Evergreen.V47.Units.ScreenCoordinate)
    | MouseMove (Evergreen.V47.Point2d.Point2d Pixels.Pixels Evergreen.V47.Units.ScreenCoordinate)
    | TouchMove (Evergreen.V47.Point2d.Point2d Pixels.Pixels Evergreen.V47.Units.ScreenCoordinate)
    | ShortIntervalElapsed Time.Posix
    | VeryShortIntervalElapsed Time.Posix
    | ZoomFactorPressed Int
    | SelectToolPressed ToolType
    | UndoPressed
    | RedoPressed
    | CopyPressed
    | CutPressed
    | UnhideUserPressed Evergreen.V47.User.UserId
    | UserTagMouseEntered Evergreen.V47.User.UserId
    | UserTagMouseExited Evergreen.V47.User.UserId
    | HideForAllTogglePressed Evergreen.V47.User.UserId
    | ToggleAdminEnabledPressed
    | HideUserPressed
        { userId : Evergreen.V47.User.UserId
        , hidePoint : Evergreen.V47.Helper.Coord Evergreen.V47.Units.AsciiUnit
        }
    | AnimationFrame Time.Posix
    | PressedCancelNotifyMe
    | PressedSubmitNotifyMe Evergreen.V47.NotifyMe.Validated
    | NotifyMeModelChanged Evergreen.V47.NotifyMe.Model


type ToBackend
    = RequestData (Evergreen.V47.Bounds.Bounds Evergreen.V47.Units.CellUnit)
    | GridChange (List.Nonempty.Nonempty Evergreen.V47.Change.LocalChange)
    | ChangeViewBounds (Evergreen.V47.Bounds.Bounds Evergreen.V47.Units.CellUnit)
    | NotifyMeSubmitted Evergreen.V47.NotifyMe.Validated
    | ConfirmationEmailConfirmed_ Evergreen.V47.UrlHelper.ConfirmEmailKey
    | UnsubscribeEmail Evergreen.V47.UrlHelper.UnsubscribeEmailKey


type BackendMsg
    = UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | NotifyAdminTimeElapsed Time.Posix
    | NotifyAdminEmailSent
    | ConfirmationEmailSent Lamdera.SessionId Time.Posix (Result SendGrid.Error ())
    | ChangeEmailSent Time.Posix Email.Email (Result SendGrid.Error ())
    | UpdateFromFrontend Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix


type alias LoadingData_ =
    { user : Evergreen.V47.User.UserId
    , grid : Evergreen.V47.Grid.Grid
    , hiddenUsers : SeqSet.SeqSet Evergreen.V47.User.UserId
    , adminHiddenUsers : SeqSet.SeqSet Evergreen.V47.User.UserId
    , undoHistory : List (Dict.Dict Evergreen.V47.Helper.RawCellCoord Int)
    , redoHistory : List (Dict.Dict Evergreen.V47.Helper.RawCellCoord Int)
    , undoCurrent : Dict.Dict Evergreen.V47.Helper.RawCellCoord Int
    , viewBounds : Evergreen.V47.Bounds.Bounds Evergreen.V47.Units.CellUnit
    }


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (List.Nonempty.Nonempty Evergreen.V47.Change.Change)
    | NotifyMeEmailSent
        { isSuccessful : Bool
        }
    | NotifyMeConfirmed
    | UnsubscribeEmailConfirmed
