module Evergreen.Migrate.V47 exposing (backendModel, backendMsg, frontendModel, frontendMsg, toBackend, toFrontend)

import AssocList
import Dict
import Evergreen.V45.Ascii
import Evergreen.V45.Grid
import Evergreen.V45.GridCell
import Evergreen.V45.Helper
import Evergreen.V45.Types as Old exposing (BackendMsg(..))
import Evergreen.V45.User
import Evergreen.V47.Ascii as Ascii
import Evergreen.V47.Grid as Grid
import Evergreen.V47.GridCell as GridCell
import Evergreen.V47.NotifyMe exposing (Frequency(..))
import Evergreen.V47.RecentChanges exposing (RecentChanges(..))
import Evergreen.V47.Types as New
import Evergreen.V47.User as User
import EverySet
import Helper
import Lamdera.Migrations exposing (..)
import List.Nonempty
import Quantity exposing (Quantity(..))


migrateUserId : Evergreen.V45.User.UserId -> User.UserId
migrateUserId (Evergreen.V45.User.UserId userId) =
    User.UserId userId


migrateAscii : Evergreen.V45.Ascii.Ascii -> Ascii.Ascii
migrateAscii (Evergreen.V45.Ascii.Ascii ascii) =
    Ascii.Ascii ascii


migrateCell : Evergreen.V45.GridCell.Cell -> GridCell.Cell
migrateCell (Evergreen.V45.GridCell.Cell cell) =
    GridCell.Cell
        { history =
            List.map
                (\{ userId, position, line } ->
                    { userId = migrateUserId userId
                    , position = position
                    , line = List.Nonempty.map migrateAscii line
                    }
                )
                cell.history
        , undoPoint = cell.undoPoint
        }


migrateGrid : Evergreen.V45.Grid.Grid -> Grid.Grid
migrateGrid (Evergreen.V45.Grid.Grid dict) =
    Dict.map (\_ v -> migrateCell v) dict |> Grid.Grid


migrateQuantity : Quantity number a -> Quantity number b
migrateQuantity (Quantity quantity) =
    Quantity quantity


migrateCoord : Evergreen.V45.Helper.Coord a -> Helper.Coord b
migrateCoord ( x, y ) =
    ( migrateQuantity x, migrateQuantity y )


recentChanges : Evergreen.V47.RecentChanges.RecentChanges
recentChanges =
    RecentChanges
        { frequencies =
            [ Every3Hours
            , Every12Hours
            , Daily
            , Weekly
            , Monthly
            ]
                |> List.map (\a -> ( a, Dict.empty ))
                |> AssocList.fromList
        , threeHoursElapsed = Quantity 1
        }


migrateBackendModel : Old.BackendModel -> New.BackendModel
migrateBackendModel old =
    { grid = migrateGrid old.grid
    , userSessions =
        Dict.map
            (\_ { clientIds, userId } ->
                { clientIds = Dict.empty
                , userId = migrateUserId userId
                }
            )
            old.userSessions
    , users =
        Dict.toList old.users
            |> List.map
                (\( userId, user ) ->
                    ( userId
                    , { hiddenUsers = EverySet.map migrateUserId user.hiddenUsers
                      , hiddenForAll = user.hiddenForAll
                      , undoHistory = []
                      , redoHistory = []
                      , undoCurrent = Dict.empty
                      }
                    )
                )
            |> Dict.fromList
    , usersHiddenRecently =
        List.map
            (\{ reporter, hiddenUser, hidePoint } ->
                { reporter = migrateUserId reporter, hiddenUser = migrateUserId hiddenUser, hidePoint = migrateCoord hidePoint }
            )
            old.usersHiddenRecently
    , userChangesRecently = recentChanges
    , pendingEmails = []
    , subscribedEmails = []
    , secretLinkCounter = 0
    , errors = []
    }


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelUnchanged


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated ( migrateBackendModel old, Cmd.none )


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgMigrated
        ( case old of
            UserDisconnected sessionId clientId ->
                New.UserDisconnected sessionId clientId

            NotifyAdminTimeElapsed time ->
                New.NotifyAdminTimeElapsed time

            NotifyAdminEmailSent ->
                New.NotifyAdminEmailSent
        , Cmd.none
        )


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged
