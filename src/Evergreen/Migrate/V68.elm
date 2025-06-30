module Evergreen.Migrate.V68 exposing (..)

import AssocList
import Dict
import EmailAddress exposing (EmailAddress)
import Evergreen.V53.Ascii
import Evergreen.V53.Email
import Evergreen.V53.Grid
import Evergreen.V53.GridCell
import Evergreen.V53.Helper
import Evergreen.V53.NotifyMe
import Evergreen.V53.Types as Old
import Evergreen.V53.UrlHelper
import Evergreen.V53.User
import Evergreen.V68.Ascii as Ascii
import Evergreen.V68.Grid as Grid
import Evergreen.V68.GridCell as GridCell
import Evergreen.V68.Helper as Helper
import Evergreen.V68.NotifyMe as NotifyMe exposing (Frequency(..))
import Evergreen.V68.RecentChanges exposing (RecentChanges(..))
import Evergreen.V68.Types as New exposing (SubscribedEmail)
import Evergreen.V68.UrlHelper exposing (UnsubscribeEmailKey(..))
import Evergreen.V68.User as User
import Lamdera.Migrations exposing (..)
import List.Nonempty
import Quantity exposing (Quantity(..))
import SeqSet


migrateUserId : Evergreen.V53.User.UserId -> User.UserId
migrateUserId (Evergreen.V53.User.UserId userId) =
    User.UserId userId


migrateUnsubscribeEmailKey : Evergreen.V53.UrlHelper.UnsubscribeEmailKey -> UnsubscribeEmailKey
migrateUnsubscribeEmailKey (Evergreen.V53.UrlHelper.UnsubscribeEmailKey key) =
    UnsubscribeEmailKey key


migrateAscii : Evergreen.V53.Ascii.Ascii -> Ascii.Ascii
migrateAscii (Evergreen.V53.Ascii.Ascii ascii) =
    Ascii.Ascii ascii


migrateCell : Evergreen.V53.GridCell.Cell -> GridCell.Cell
migrateCell (Evergreen.V53.GridCell.Cell cell) =
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


migrateGrid : Evergreen.V53.Grid.Grid -> Grid.Grid
migrateGrid (Evergreen.V53.Grid.Grid dict) =
    Dict.map (\_ v -> migrateCell v) dict |> Grid.Grid


migrateQuantity : Quantity number a -> Quantity number b
migrateQuantity (Quantity quantity) =
    Quantity quantity


migrateCoord : Evergreen.V53.Helper.Coord a -> Helper.Coord b
migrateCoord ( x, y ) =
    ( migrateQuantity x, migrateQuantity y )


recentChanges : Evergreen.V68.RecentChanges.RecentChanges
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


migrateFrequency : Evergreen.V53.NotifyMe.Frequency -> NotifyMe.Frequency
migrateFrequency old =
    case old of
        Evergreen.V53.NotifyMe.Every3Hours ->
            Every3Hours

        Evergreen.V53.NotifyMe.Every12Hours ->
            Every12Hours

        Evergreen.V53.NotifyMe.Daily ->
            Daily

        Evergreen.V53.NotifyMe.Weekly ->
            Weekly

        Evergreen.V53.NotifyMe.Monthly ->
            Monthly


emailToString : Evergreen.V53.Email.Email -> String
emailToString { localPart, tags, domain, tld } =
    String.join ""
        [ localPart
        , case tags of
            [] ->
                ""

            _ ->
                "+" ++ String.join "+" tags
        , "@"
        , domain
        , "."
        , String.join "." tld
        ]


migrateEmailAddress : Evergreen.V53.Email.Email -> Maybe EmailAddress
migrateEmailAddress email =
    emailToString email |> EmailAddress.fromString


migratedSubscribedEmail : Old.SubscribedEmail -> Maybe SubscribedEmail
migratedSubscribedEmail old =
    case migrateEmailAddress old.email of
        Just emailAddress ->
            { email = emailAddress
            , frequency = migrateFrequency old.frequency
            , confirmTime = old.confirmTime
            , userId = migrateUserId old.userId
            , unsubscribeKey = migrateUnsubscribeEmailKey old.unsubscribeKey
            }
                |> Just

        Nothing ->
            Nothing


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
                    , { hiddenUsers = SeqSet.map migrateUserId user.hiddenUsers
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
    , pendingEmails = []
    , subscribedEmails = List.filterMap migratedSubscribedEmail old.subscribedEmails
    , secretLinkCounter = old.secretLinkCounter
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
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged
