module Evergreen.Migrate.V17 exposing (..)

import Dict
import Evergreen.V14.Ascii
import Evergreen.V14.Grid
import Evergreen.V14.GridCell
import Evergreen.V14.Helper
import Evergreen.V14.Types as Old
import Evergreen.V14.User
import Evergreen.V17.Ascii
import Evergreen.V17.Grid
import Evergreen.V17.GridCell
import Evergreen.V17.Helper
import Evergreen.V17.Types as New
import Evergreen.V17.User
import EverySet
import Lamdera.Migrations exposing (..)
import List.Nonempty
import Quantity exposing (Quantity(..))


migrateUserId : Evergreen.V14.User.UserId -> Evergreen.V17.User.UserId
migrateUserId (Evergreen.V14.User.UserId userId) =
    Evergreen.V17.User.UserId userId


migrateAscii : Evergreen.V14.Ascii.Ascii -> Evergreen.V17.Ascii.Ascii
migrateAscii (Evergreen.V14.Ascii.Ascii ascii) =
    Evergreen.V17.Ascii.Ascii ascii


migrateCell : Evergreen.V14.GridCell.Cell -> Evergreen.V17.GridCell.Cell
migrateCell (Evergreen.V14.GridCell.Cell cell) =
    Evergreen.V17.GridCell.Cell
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


migrateGrid : Evergreen.V14.Grid.Grid -> Evergreen.V17.Grid.Grid
migrateGrid (Evergreen.V14.Grid.Grid dict) =
    Dict.map (\_ v -> migrateCell v) dict |> Evergreen.V17.Grid.Grid


migrateQuantity : Quantity number a -> Quantity number b
migrateQuantity (Quantity quantity) =
    Quantity quantity


migrateCoord : Evergreen.V14.Helper.Coord a -> Evergreen.V17.Helper.Coord b
migrateCoord ( x, y ) =
    ( migrateQuantity x, migrateQuantity y )


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
    , userChangesRecently = Dict.empty
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
