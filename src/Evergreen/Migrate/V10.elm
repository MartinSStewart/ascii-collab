module Evergreen.Migrate.V10 exposing (..)

import Dict
import Evergreen.V10.Ascii
import Evergreen.V10.ColorIndex
import Evergreen.V10.Grid
import Evergreen.V10.GridCell
import Evergreen.V10.Types as New
import Evergreen.V10.User
import Evergreen.V8.Ascii
import Evergreen.V8.ColorIndex
import Evergreen.V8.Grid
import Evergreen.V8.GridCell
import Evergreen.V8.Types as Old
import Evergreen.V8.User
import EverySet
import Lamdera.Migrations exposing (..)
import List.Nonempty


migrateUserId : Evergreen.V8.User.UserId -> Evergreen.V10.User.UserId
migrateUserId (Evergreen.V8.User.UserId userId) =
    Evergreen.V10.User.UserId userId


migrateAscii : Evergreen.V8.Ascii.Ascii -> Evergreen.V10.Ascii.Ascii
migrateAscii (Evergreen.V8.Ascii.Ascii ascii) =
    Evergreen.V10.Ascii.Ascii ascii


migrateCell : Evergreen.V8.GridCell.Cell -> Evergreen.V10.GridCell.Cell
migrateCell (Evergreen.V8.GridCell.Cell cell) =
    Evergreen.V10.GridCell.Cell
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


migrateGrid : Evergreen.V8.Grid.Grid -> Evergreen.V10.Grid.Grid
migrateGrid (Evergreen.V8.Grid.Grid dict) =
    Dict.map (\_ v -> migrateCell v) dict |> Evergreen.V10.Grid.Grid


migrateColorIndex : Evergreen.V8.ColorIndex.ColorIndex -> Evergreen.V10.ColorIndex.ColorIndex
migrateColorIndex colorIndex =
    case colorIndex of
        Evergreen.V8.ColorIndex.Green ->
            Evergreen.V10.ColorIndex.Green

        Evergreen.V8.ColorIndex.Teal ->
            Evergreen.V10.ColorIndex.Teal

        Evergreen.V8.ColorIndex.Blue ->
            Evergreen.V10.ColorIndex.Blue

        Evergreen.V8.ColorIndex.Purple ->
            Evergreen.V10.ColorIndex.Purple

        Evergreen.V8.ColorIndex.Magenta ->
            Evergreen.V10.ColorIndex.Magenta

        Evergreen.V8.ColorIndex.Salmon ->
            Evergreen.V10.ColorIndex.Salmon

        Evergreen.V8.ColorIndex.Orange ->
            Evergreen.V10.ColorIndex.Orange

        Evergreen.V8.ColorIndex.Yellow ->
            Evergreen.V10.ColorIndex.Yellow

        Evergreen.V8.ColorIndex.Gray ->
            Evergreen.V10.ColorIndex.Gray


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
                    , { userData =
                            user.userData
                                |> (\(Evergreen.V8.User.User { color }) -> color)
                                |> migrateColorIndex
                                |> (\a -> Evergreen.V10.User.User { color = a })
                      , hiddenUsers = EverySet.empty
                      , hiddenForAll = False
                      , undoHistory = []
                      , redoHistory = []
                      , undoCurrent = Dict.empty
                      }
                    )
                )
            |> Dict.fromList
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
