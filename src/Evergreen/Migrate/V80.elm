module Evergreen.Migrate.V80 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import AssocList
import Dict
import Evergreen.V79.Ascii
import Evergreen.V79.Bounds
import Evergreen.V79.Change
import Evergreen.V79.Geometry.Types
import Evergreen.V79.Grid
import Evergreen.V79.GridCell
import Evergreen.V79.Helper
import Evergreen.V79.NotifyMe
import Evergreen.V79.Point2d
import Evergreen.V79.Types
import Evergreen.V79.Units
import Evergreen.V79.UrlHelper
import Evergreen.V79.User
import Evergreen.V80.Ascii
import Evergreen.V80.Bounds
import Evergreen.V80.Change
import Evergreen.V80.Geometry.Types
import Evergreen.V80.Grid
import Evergreen.V80.GridCell
import Evergreen.V80.Helper
import Evergreen.V80.NotifyMe
import Evergreen.V80.Point2d
import Evergreen.V80.Types
import Evergreen.V80.Units
import Evergreen.V80.UrlHelper
import Evergreen.V80.User
import EverySet
import Lamdera.Migrations exposing (..)
import List.Nonempty
import Maybe
import Quantity


frontendModel : Evergreen.V79.Types.FrontendModel -> ModelMigration Evergreen.V80.Types.FrontendModel Evergreen.V80.Types.FrontendMsg
frontendModel old =
    ModelUnchanged


backendModel : Evergreen.V79.Types.BackendModel -> ModelMigration Evergreen.V80.Types.BackendModel Evergreen.V80.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V79.Types.FrontendMsg -> MsgMigration Evergreen.V80.Types.FrontendMsg Evergreen.V80.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V79.Types.ToBackend -> MsgMigration Evergreen.V80.Types.ToBackend Evergreen.V80.Types.BackendMsg
toBackend old =
    MsgMigrated ( migrate_Types_ToBackend old, Cmd.none )


backendMsg : Evergreen.V79.Types.BackendMsg -> MsgMigration Evergreen.V80.Types.BackendMsg Evergreen.V80.Types.BackendMsg
backendMsg old =
    MsgMigrated ( migrate_Types_BackendMsg old, Cmd.none )


toFrontend : Evergreen.V79.Types.ToFrontend -> MsgMigration Evergreen.V80.Types.ToFrontend Evergreen.V80.Types.FrontendMsg
toFrontend old =
    MsgMigrated ( migrate_Types_ToFrontend old, Cmd.none )


migrate_Ascii_Ascii : Evergreen.V79.Ascii.Ascii -> Evergreen.V80.Ascii.Ascii
migrate_Ascii_Ascii old =
    case old of
        Evergreen.V79.Ascii.Ascii p0 ->
            Evergreen.V80.Ascii.Ascii p0


migrate_AssocList_Dict : (a_old -> a_new) -> (b_old -> b_new) -> AssocList.Dict a_old b_old -> AssocList.Dict a_new b_new
migrate_AssocList_Dict migrate_a migrate_b old =
    old
        |> AssocList.toList
        |> List.map (Tuple.mapBoth migrate_a migrate_b)
        |> AssocList.fromList


migrate_Bounds_Bounds : (unit_old -> unit_new) -> Evergreen.V79.Bounds.Bounds unit_old -> Evergreen.V80.Bounds.Bounds unit_new
migrate_Bounds_Bounds migrate_unit old =
    case old of
        Evergreen.V79.Bounds.Bounds p0 ->
            Evergreen.V80.Bounds.Bounds
                { min = p0.min |> migrate_Helper_Coord migrate_unit
                , max = p0.max |> migrate_Helper_Coord migrate_unit
                }


migrate_Change_Change : Evergreen.V79.Change.Change -> Evergreen.V80.Change.Change
migrate_Change_Change old =
    case old of
        Evergreen.V79.Change.LocalChange p0 ->
            Evergreen.V80.Change.LocalChange (p0 |> migrate_Change_LocalChange)

        Evergreen.V79.Change.ServerChange p0 ->
            Evergreen.V80.Change.ServerChange (p0 |> migrate_Change_ServerChange)

        Evergreen.V79.Change.ClientChange p0 ->
            Evergreen.V80.Change.ClientChange (p0 |> migrate_Change_ClientChange)


migrate_Change_ClientChange : Evergreen.V79.Change.ClientChange -> Evergreen.V80.Change.ClientChange
migrate_Change_ClientChange old =
    case old of
        Evergreen.V79.Change.ViewBoundsChange p0 p1 ->
            Evergreen.V80.Change.ViewBoundsChange (p0 |> migrate_Bounds_Bounds migrate_Units_CellUnit)
                (p1 |> List.map (Tuple.mapBoth (migrate_Helper_Coord migrate_Units_CellUnit) migrate_GridCell_Cell))


migrate_Change_LocalChange : Evergreen.V79.Change.LocalChange -> Evergreen.V80.Change.LocalChange
migrate_Change_LocalChange old =
    case old of
        Evergreen.V79.Change.LocalGridChange p0 ->
            Evergreen.V80.Change.LocalGridChange (p0 |> migrate_Grid_LocalGridChange)

        Evergreen.V79.Change.LocalUndo ->
            Evergreen.V80.Change.LocalUndo

        Evergreen.V79.Change.LocalRedo ->
            Evergreen.V80.Change.LocalRedo

        Evergreen.V79.Change.LocalAddUndo ->
            Evergreen.V80.Change.LocalAddUndo

        Evergreen.V79.Change.LocalHideUser p0 p1 ->
            Evergreen.V80.Change.LocalHideUser (p0 |> migrate_User_UserId)
                (p1 |> migrate_Helper_Coord migrate_Units_AsciiUnit)

        Evergreen.V79.Change.LocalUnhideUser p0 ->
            Evergreen.V80.Change.LocalUnhideUser (p0 |> migrate_User_UserId)

        Evergreen.V79.Change.LocalToggleUserVisibilityForAll p0 ->
            Evergreen.V80.Change.LocalToggleUserVisibilityForAll (p0 |> migrate_User_UserId)


migrate_Change_ServerChange : Evergreen.V79.Change.ServerChange -> Evergreen.V80.Change.ServerChange
migrate_Change_ServerChange old =
    case old of
        Evergreen.V79.Change.ServerGridChange p0 ->
            Evergreen.V80.Change.ServerGridChange (p0 |> migrate_Grid_GridChange)

        Evergreen.V79.Change.ServerUndoPoint p0 ->
            Evergreen.V80.Change.ServerUndoPoint
                { userId = p0.userId |> migrate_User_UserId
                , undoPoints = p0.undoPoints
                }

        Evergreen.V79.Change.ServerToggleUserVisibilityForAll p0 ->
            Evergreen.V80.Change.ServerToggleUserVisibilityForAll (p0 |> migrate_User_UserId)


migrate_EverySet_EverySet : (a_old -> a_new) -> EverySet.EverySet a_old -> EverySet.EverySet a_new
migrate_EverySet_EverySet migrate_a old =
    EverySet.map migrate_a old


migrate_Geometry_Types_Point2d : Evergreen.V79.Geometry.Types.Point2d units_old coordinates_old -> Evergreen.V80.Geometry.Types.Point2d units_new coordinates_new
migrate_Geometry_Types_Point2d old =
    case old of
        Evergreen.V79.Geometry.Types.Point2d p0 ->
            Evergreen.V80.Geometry.Types.Point2d p0


migrate_GridCell_Cell : Evergreen.V79.GridCell.Cell -> Evergreen.V80.GridCell.Cell
migrate_GridCell_Cell old =
    case old of
        Evergreen.V79.GridCell.Cell p0 ->
            Evergreen.V80.GridCell.Cell
                { history =
                    p0.history
                        |> List.map
                            (\rec1 ->
                                { userId = rec1.userId |> migrate_User_UserId
                                , position = rec1.position
                                , line = rec1.line |> migrate_List_Nonempty_Nonempty migrate_Ascii_Ascii
                                }
                            )
                , undoPoint = p0.undoPoint
                }


migrate_Grid_Grid : Evergreen.V79.Grid.Grid -> Evergreen.V80.Grid.Grid
migrate_Grid_Grid old =
    case old of
        Evergreen.V79.Grid.Grid p0 ->
            Evergreen.V80.Grid.Grid (p0 |> Dict.map (\k -> migrate_GridCell_Cell))


migrate_Grid_GridChange : Evergreen.V79.Grid.GridChange -> Evergreen.V80.Grid.GridChange
migrate_Grid_GridChange old =
    { cellPosition = old.cellPosition |> migrate_Helper_Coord migrate_Units_CellUnit
    , localPosition = old.localPosition
    , change = old.change |> migrate_List_Nonempty_Nonempty migrate_Ascii_Ascii
    , userId = old.userId |> migrate_User_UserId
    }


migrate_Grid_LocalGridChange : Evergreen.V79.Grid.LocalGridChange -> Evergreen.V80.Grid.LocalGridChange
migrate_Grid_LocalGridChange old =
    { cellPosition = old.cellPosition |> migrate_Helper_Coord migrate_Units_CellUnit
    , localPosition = old.localPosition
    , change = old.change |> migrate_List_Nonempty_Nonempty migrate_Ascii_Ascii
    }


migrate_Helper_Coord : (units_old -> units_new) -> Evergreen.V79.Helper.Coord units_old -> Evergreen.V80.Helper.Coord units_new
migrate_Helper_Coord migrate_units old =
    old |> Tuple.mapBoth migrate_Quantity_Quantity migrate_Quantity_Quantity


migrate_List_Nonempty_Nonempty : (a_old -> a_new) -> List.Nonempty.Nonempty a_old -> List.Nonempty.Nonempty a_new
migrate_List_Nonempty_Nonempty migrate_a old =
    old |> List.Nonempty.map migrate_a


migrate_NotifyMe_Frequency : Evergreen.V79.NotifyMe.Frequency -> Evergreen.V80.NotifyMe.Frequency
migrate_NotifyMe_Frequency old =
    case old of
        Evergreen.V79.NotifyMe.Every3Hours ->
            Evergreen.V80.NotifyMe.Every3Hours

        Evergreen.V79.NotifyMe.Every12Hours ->
            Evergreen.V80.NotifyMe.Every12Hours

        Evergreen.V79.NotifyMe.Daily ->
            Evergreen.V80.NotifyMe.Daily

        Evergreen.V79.NotifyMe.Weekly ->
            Evergreen.V80.NotifyMe.Weekly

        Evergreen.V79.NotifyMe.Monthly ->
            Evergreen.V80.NotifyMe.Monthly


migrate_NotifyMe_InProgressModel : Evergreen.V79.NotifyMe.InProgressModel -> Evergreen.V80.NotifyMe.InProgressModel
migrate_NotifyMe_InProgressModel old =
    { status = old.status |> migrate_NotifyMe_Status
    , email = old.email
    , frequency = old.frequency |> Maybe.map migrate_NotifyMe_Frequency
    }


migrate_NotifyMe_Model : Evergreen.V79.NotifyMe.Model -> Evergreen.V80.NotifyMe.Model
migrate_NotifyMe_Model old =
    case old of
        Evergreen.V79.NotifyMe.InProgress p0 ->
            Evergreen.V80.NotifyMe.InProgress (p0 |> migrate_NotifyMe_InProgressModel)

        Evergreen.V79.NotifyMe.Completed ->
            Evergreen.V80.NotifyMe.Completed

        Evergreen.V79.NotifyMe.BackendError ->
            Evergreen.V80.NotifyMe.BackendError

        Evergreen.V79.NotifyMe.Unsubscribing ->
            Evergreen.V80.NotifyMe.Unsubscribing

        Evergreen.V79.NotifyMe.Unsubscribed ->
            Evergreen.V80.NotifyMe.Unsubscribed


migrate_NotifyMe_Status : Evergreen.V79.NotifyMe.Status -> Evergreen.V80.NotifyMe.Status
migrate_NotifyMe_Status old =
    case old of
        Evergreen.V79.NotifyMe.Form ->
            Evergreen.V80.NotifyMe.Form

        Evergreen.V79.NotifyMe.FormWithError ->
            Evergreen.V80.NotifyMe.FormWithError

        Evergreen.V79.NotifyMe.SendingToBackend ->
            Evergreen.V80.NotifyMe.SendingToBackend

        Evergreen.V79.NotifyMe.WaitingOnConfirmation ->
            Evergreen.V80.NotifyMe.WaitingOnConfirmation


migrate_NotifyMe_Validated : Evergreen.V79.NotifyMe.Validated -> Evergreen.V80.NotifyMe.Validated
migrate_NotifyMe_Validated old =
    { email = old.email
    , frequency = old.frequency |> migrate_NotifyMe_Frequency
    }


migrate_Point2d_Point2d : (units_old -> units_new) -> (coordinates_old -> coordinates_new) -> Evergreen.V79.Point2d.Point2d units_old coordinates_old -> Evergreen.V80.Point2d.Point2d units_new coordinates_new
migrate_Point2d_Point2d migrate_units migrate_coordinates old =
    old |> migrate_Geometry_Types_Point2d


migrate_Quantity_Quantity : Quantity.Quantity number units -> Quantity.Quantity number units2
migrate_Quantity_Quantity old =
    Quantity.unwrap old |> Quantity.unsafe


migrate_Types_BackendMsg : Evergreen.V79.Types.BackendMsg -> Evergreen.V80.Types.BackendMsg
migrate_Types_BackendMsg old =
    case old of
        Evergreen.V79.Types.UserDisconnected p0 p1 ->
            Evergreen.V80.Types.UserDisconnected p0 p1

        Evergreen.V79.Types.NotifyAdminTimeElapsed p0 ->
            Evergreen.V80.Types.NotifyAdminTimeElapsed p0

        Evergreen.V79.Types.NotifyAdminEmailSent ->
            Evergreen.V80.Types.NotifyAdminEmailSent

        Evergreen.V79.Types.ConfirmationEmailSent p0 p1 p2 ->
            Evergreen.V80.Types.ConfirmationEmailSent p0 p1 p2

        Evergreen.V79.Types.ChangeEmailSent p0 p1 p2 ->
            Evergreen.V80.Types.ChangeEmailSent p0 p1 p2

        Evergreen.V79.Types.UpdateFromFrontend p0 p1 p2 p3 ->
            Evergreen.V80.Types.UpdateFromFrontend p0 p1 (p2 |> migrate_Types_ToBackend) p3


migrate_Types_EmailEvent : Evergreen.V79.Types.EmailEvent -> Evergreen.V80.Types.EmailEvent
migrate_Types_EmailEvent old =
    case old of
        Evergreen.V79.Types.ConfirmationEmailConfirmed_ p0 ->
            Evergreen.V80.Types.ConfirmationEmailConfirmed_ (p0 |> migrate_UrlHelper_ConfirmEmailKey)

        Evergreen.V79.Types.UnsubscribeEmail p0 ->
            Evergreen.V80.Types.UnsubscribeEmail (p0 |> migrate_UrlHelper_UnsubscribeEmailKey)


migrate_Types_FrontendMsg : Evergreen.V79.Types.FrontendMsg -> Evergreen.V80.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V79.Types.UrlClicked p0 ->
            Evergreen.V80.Types.UrlClicked p0

        Evergreen.V79.Types.UrlChanged p0 ->
            Evergreen.V80.Types.UrlChanged p0

        Evergreen.V79.Types.NoOpFrontendMsg ->
            Evergreen.V80.Types.NoOpFrontendMsg

        Evergreen.V79.Types.TextureLoaded p0 ->
            Evergreen.V80.Types.TextureLoaded p0

        Evergreen.V79.Types.KeyMsg p0 ->
            Evergreen.V80.Types.KeyMsg p0

        Evergreen.V79.Types.KeyDown p0 ->
            Evergreen.V80.Types.KeyDown p0

        Evergreen.V79.Types.WindowResized p0 ->
            Evergreen.V80.Types.WindowResized p0

        Evergreen.V79.Types.GotDevicePixelRatio p0 ->
            Evergreen.V80.Types.GotDevicePixelRatio (p0 |> migrate_Quantity_Quantity)

        Evergreen.V79.Types.UserTyped p0 ->
            Evergreen.V80.Types.UserTyped p0

        Evergreen.V79.Types.TextAreaFocused ->
            Evergreen.V80.Types.TextAreaFocused

        Evergreen.V79.Types.MouseDown p0 p1 ->
            Evergreen.V80.Types.MouseDown p0
                (p1 |> migrate_Point2d_Point2d identity migrate_Units_ScreenCoordinate)

        Evergreen.V79.Types.MouseUp p0 p1 ->
            Evergreen.V80.Types.MouseUp p0
                (p1 |> migrate_Point2d_Point2d identity migrate_Units_ScreenCoordinate)

        Evergreen.V79.Types.MouseMove p0 ->
            Evergreen.V80.Types.MouseMove (p0 |> migrate_Point2d_Point2d identity migrate_Units_ScreenCoordinate)

        Evergreen.V79.Types.TouchMove p0 ->
            Evergreen.V80.Types.TouchMove (p0 |> migrate_Point2d_Point2d identity migrate_Units_ScreenCoordinate)

        Evergreen.V79.Types.ShortIntervalElapsed p0 ->
            Evergreen.V80.Types.ShortIntervalElapsed p0

        Evergreen.V79.Types.VeryShortIntervalElapsed p0 ->
            Evergreen.V80.Types.VeryShortIntervalElapsed p0

        Evergreen.V79.Types.ZoomFactorPressed p0 ->
            Evergreen.V80.Types.ZoomFactorPressed p0

        Evergreen.V79.Types.SelectToolPressed p0 ->
            Evergreen.V80.Types.SelectToolPressed (p0 |> migrate_Types_ToolType)

        Evergreen.V79.Types.UndoPressed ->
            Evergreen.V80.Types.UndoPressed

        Evergreen.V79.Types.RedoPressed ->
            Evergreen.V80.Types.RedoPressed

        Evergreen.V79.Types.CopyPressed ->
            Evergreen.V80.Types.CopyPressed

        Evergreen.V79.Types.CutPressed ->
            Evergreen.V80.Types.CutPressed

        Evergreen.V79.Types.UnhideUserPressed p0 ->
            Evergreen.V80.Types.UnhideUserPressed (p0 |> migrate_User_UserId)

        Evergreen.V79.Types.UserTagMouseEntered p0 ->
            Evergreen.V80.Types.UserTagMouseEntered (p0 |> migrate_User_UserId)

        Evergreen.V79.Types.UserTagMouseExited p0 ->
            Evergreen.V80.Types.UserTagMouseExited (p0 |> migrate_User_UserId)

        Evergreen.V79.Types.HideForAllTogglePressed p0 ->
            Evergreen.V80.Types.HideForAllTogglePressed (p0 |> migrate_User_UserId)

        Evergreen.V79.Types.ToggleAdminEnabledPressed ->
            Evergreen.V80.Types.ToggleAdminEnabledPressed

        Evergreen.V79.Types.HideUserPressed p0 ->
            Evergreen.V80.Types.HideUserPressed
                { userId = p0.userId |> migrate_User_UserId
                , hidePoint = p0.hidePoint |> migrate_Helper_Coord migrate_Units_AsciiUnit
                }

        Evergreen.V79.Types.AnimationFrame p0 ->
            Evergreen.V80.Types.AnimationFrame p0

        Evergreen.V79.Types.PressedCancelNotifyMe ->
            Evergreen.V80.Types.PressedCancelNotifyMe

        Evergreen.V79.Types.PressedSubmitNotifyMe p0 ->
            Evergreen.V80.Types.PressedSubmitNotifyMe (p0 |> migrate_NotifyMe_Validated)

        Evergreen.V79.Types.NotifyMeModelChanged p0 ->
            Evergreen.V80.Types.NotifyMeModelChanged (p0 |> migrate_NotifyMe_Model)


migrate_Types_LoadingData_ : Evergreen.V79.Types.LoadingData_ -> Evergreen.V80.Types.LoadingData_
migrate_Types_LoadingData_ old =
    { user = old.user |> migrate_User_UserId
    , grid = old.grid |> migrate_Grid_Grid
    , hiddenUsers = old.hiddenUsers |> migrate_EverySet_EverySet migrate_User_UserId
    , adminHiddenUsers = old.adminHiddenUsers |> migrate_EverySet_EverySet migrate_User_UserId
    , undoHistory = old.undoHistory
    , redoHistory = old.redoHistory
    , undoCurrent = old.undoCurrent
    , viewBounds = old.viewBounds |> migrate_Bounds_Bounds migrate_Units_CellUnit
    }


migrate_Types_ToBackend : Evergreen.V79.Types.ToBackend -> Evergreen.V80.Types.ToBackend
migrate_Types_ToBackend old =
    case old of
        Evergreen.V79.Types.ConnectToBackend p0 p1 ->
            Evergreen.V80.Types.ConnectToBackend (p0 |> migrate_Bounds_Bounds migrate_Units_CellUnit)
                (p1 |> Maybe.map migrate_Types_EmailEvent)

        Evergreen.V79.Types.GridChange p0 ->
            Evergreen.V80.Types.GridChange (p0 |> migrate_List_Nonempty_Nonempty migrate_Change_LocalChange)

        Evergreen.V79.Types.ChangeViewBounds p0 ->
            Evergreen.V80.Types.ChangeViewBounds (p0 |> migrate_Bounds_Bounds migrate_Units_CellUnit)

        Evergreen.V79.Types.NotifyMeSubmitted p0 ->
            Evergreen.V80.Types.NotifyMeSubmitted (p0 |> migrate_NotifyMe_Validated)


migrate_Types_ToFrontend : Evergreen.V79.Types.ToFrontend -> Evergreen.V80.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V79.Types.LoadingData p0 ->
            Evergreen.V80.Types.LoadingData (p0 |> migrate_Types_LoadingData_)

        Evergreen.V79.Types.ChangeBroadcast p0 ->
            Evergreen.V80.Types.ChangeBroadcast (p0 |> migrate_List_Nonempty_Nonempty migrate_Change_Change)

        Evergreen.V79.Types.NotifyMeEmailSent p0 ->
            Evergreen.V80.Types.NotifyMeEmailSent p0

        Evergreen.V79.Types.NotifyMeConfirmed ->
            Evergreen.V80.Types.NotifyMeConfirmed

        Evergreen.V79.Types.UnsubscribeEmailConfirmed ->
            Evergreen.V80.Types.UnsubscribeEmailConfirmed


migrate_Types_ToolType : Evergreen.V79.Types.ToolType -> Evergreen.V80.Types.ToolType
migrate_Types_ToolType old =
    case old of
        Evergreen.V79.Types.DragTool ->
            Evergreen.V80.Types.DragTool

        Evergreen.V79.Types.SelectTool ->
            Evergreen.V80.Types.SelectTool

        Evergreen.V79.Types.HighlightTool p0 ->
            Evergreen.V80.Types.HighlightTool (p0 |> Maybe.map (Tuple.mapBoth migrate_User_UserId (migrate_Helper_Coord migrate_Units_AsciiUnit)))


migrate_Units_AsciiUnit : Evergreen.V79.Units.AsciiUnit -> Evergreen.V80.Units.AsciiUnit
migrate_Units_AsciiUnit old =
    case old of
        Evergreen.V79.Units.AsciiUnit p0 ->
            Evergreen.V80.Units.AsciiUnit p0


migrate_Units_CellUnit : Evergreen.V79.Units.CellUnit -> Evergreen.V80.Units.CellUnit
migrate_Units_CellUnit old =
    case old of
        Evergreen.V79.Units.CellUnit p0 ->
            Evergreen.V80.Units.CellUnit p0


migrate_Units_ScreenCoordinate : Evergreen.V79.Units.ScreenCoordinate -> Evergreen.V80.Units.ScreenCoordinate
migrate_Units_ScreenCoordinate old =
    case old of
        Evergreen.V79.Units.ScreenCoordinate p0 ->
            Evergreen.V80.Units.ScreenCoordinate p0


migrate_UrlHelper_ConfirmEmailKey : Evergreen.V79.UrlHelper.ConfirmEmailKey -> Evergreen.V80.UrlHelper.ConfirmEmailKey
migrate_UrlHelper_ConfirmEmailKey old =
    case old of
        Evergreen.V79.UrlHelper.ConfirmEmailKey p0 ->
            Evergreen.V80.UrlHelper.ConfirmEmailKey p0


migrate_UrlHelper_UnsubscribeEmailKey : Evergreen.V79.UrlHelper.UnsubscribeEmailKey -> Evergreen.V80.UrlHelper.UnsubscribeEmailKey
migrate_UrlHelper_UnsubscribeEmailKey old =
    case old of
        Evergreen.V79.UrlHelper.UnsubscribeEmailKey p0 ->
            Evergreen.V80.UrlHelper.UnsubscribeEmailKey p0


migrate_User_UserId : Evergreen.V79.User.UserId -> Evergreen.V80.User.UserId
migrate_User_UserId old =
    case old of
        Evergreen.V79.User.UserId p0 ->
            Evergreen.V80.User.UserId p0
