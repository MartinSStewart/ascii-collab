module Evergreen.Migrate.V78 exposing (..)

import AssocList
import Dict
import Evergreen.V77.Ascii
import Evergreen.V77.Grid
import Evergreen.V77.GridCell
import Evergreen.V77.Helper
import Evergreen.V77.NotifyMe
import Evergreen.V77.Types as Old
import Evergreen.V77.UrlHelper
import Evergreen.V77.User
import Evergreen.V78.Ascii as Ascii
import Evergreen.V78.Grid as Grid
import Evergreen.V78.GridCell as GridCell
import Evergreen.V78.Helper as Helper
import Evergreen.V78.NotifyMe as NotifyMe exposing (Frequency(..))
import Evergreen.V78.RecentChanges exposing (RecentChanges(..))
import Evergreen.V78.Types as New exposing (SubscribedEmail)
import Evergreen.V78.UrlHelper exposing (UnsubscribeEmailKey(..))
import Evergreen.V78.User as User
import EverySet exposing (EverySet)
import Lamdera.Migrations exposing (..)
import List.Nonempty
import Quantity exposing (Quantity(..))


migrateUserId : Evergreen.V77.User.UserId -> User.UserId
migrateUserId (Evergreen.V77.User.UserId userId) =
    User.UserId userId


migrateUnsubscribeEmailKey : Evergreen.V77.UrlHelper.UnsubscribeEmailKey -> UnsubscribeEmailKey
migrateUnsubscribeEmailKey (Evergreen.V77.UrlHelper.UnsubscribeEmailKey key) =
    UnsubscribeEmailKey key


migrateAscii : Evergreen.V77.Ascii.Ascii -> Ascii.Ascii
migrateAscii (Evergreen.V77.Ascii.Ascii ascii) =
    Ascii.Ascii ascii


migrateCell : Evergreen.V77.GridCell.Cell -> GridCell.Cell
migrateCell (Evergreen.V77.GridCell.Cell cell) =
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


migrateGrid : Evergreen.V77.Grid.Grid -> Grid.Grid
migrateGrid (Evergreen.V77.Grid.Grid dict) =
    Dict.map (\_ v -> migrateCell v) dict |> Grid.Grid


migrateQuantity : Quantity number a -> Quantity number b
migrateQuantity (Quantity quantity) =
    Quantity quantity


migrateCoord : Evergreen.V77.Helper.Coord a -> Helper.Coord b
migrateCoord ( x, y ) =
    ( migrateQuantity x, migrateQuantity y )


recentChanges : Evergreen.V78.RecentChanges.RecentChanges
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


migrateFrequency : Evergreen.V77.NotifyMe.Frequency -> NotifyMe.Frequency
migrateFrequency old =
    case old of
        Evergreen.V77.NotifyMe.Every3Hours ->
            Every3Hours

        Evergreen.V77.NotifyMe.Every12Hours ->
            Every12Hours

        Evergreen.V77.NotifyMe.Daily ->
            Daily

        Evergreen.V77.NotifyMe.Weekly ->
            Weekly

        Evergreen.V77.NotifyMe.Monthly ->
            Monthly


migratedSubscribedEmail : Old.SubscribedEmail -> SubscribedEmail
migratedSubscribedEmail old =
    { email = old.email
    , frequency = migrateFrequency old.frequency
    , confirmTime = old.confirmTime
    , userId = migrateUserId old.userId
    , unsubscribeKey = migrateUnsubscribeEmailKey old.unsubscribeKey
    }


removeUser : User.UserId -> Grid.Grid -> Grid.Grid
removeUser userId (Grid.Grid grid) =
    grid
        |> Dict.map (\_ cell -> cellRemoveUser userId cell)
        |> Grid.Grid


cellRemoveUser : User.UserId -> GridCell.Cell -> GridCell.Cell
cellRemoveUser userId (GridCell.Cell cell) =
    GridCell.Cell
        { history = List.filter (.userId >> (/=) userId) cell.history
        , undoPoint = Dict.remove (rawUserId userId) cell.undoPoint
        }


rawUserId : User.UserId -> Int
rawUserId (User.UserId userId_) =
    userId_


migrateBackendModel : Old.BackendModel -> New.BackendModel
migrateBackendModel old =
    let
        adminHiddenUsers : List User.UserId
        adminHiddenUsers =
            Dict.toList old.users
                |> List.filter (\( _, user ) -> user.hiddenForAll)
                |> List.map (Tuple.first >> User.UserId)
    in
    { grid = List.foldl removeUser (migrateGrid old.grid) adminHiddenUsers
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
                { reporter = migrateUserId reporter
                , hiddenUser = migrateUserId hiddenUser
                , hidePoint = migrateCoord hidePoint
                }
            )
            old.usersHiddenRecently
    , userChangesRecently = recentChanges
    , pendingEmails = List.map migratePendingEmail old.pendingEmails
    , subscribedEmails = List.map migratedSubscribedEmail old.subscribedEmails
    , secretLinkCounter = old.secretLinkCounter
    , errors = []
    }


migratePendingEmail : Old.PendingEmail -> New.PendingEmail
migratePendingEmail old =
    { email = old.email
    , frequency = migrateFrequency old.frequency
    , creationTime = old.creationTime
    , userId = migrateUserId old.userId
    , key = migrateConfirmEmailKey old.key
    }


migrateConfirmEmailKey : Evergreen.V77.UrlHelper.ConfirmEmailKey -> Evergreen.V78.UrlHelper.ConfirmEmailKey
migrateConfirmEmailKey (Evergreen.V77.UrlHelper.ConfirmEmailKey old) =
    Evergreen.V78.UrlHelper.ConfirmEmailKey old


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
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged
