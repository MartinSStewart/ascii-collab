module Evergreen.Migrate.V12 exposing (..)

import Dict
import Evergreen.V10.Ascii
import Evergreen.V10.ColorIndex
import Evergreen.V10.Grid
import Evergreen.V10.GridCell
import Evergreen.V10.Types as Old
import Evergreen.V10.User
import Evergreen.V12.Ascii
import Evergreen.V12.ColorIndex
import Evergreen.V12.Grid
import Evergreen.V12.GridCell
import Evergreen.V12.Types as New
import Evergreen.V12.User
import Lamdera.Migrations exposing (..)
import List.Nonempty
import SeqSet


migrateUserId : Evergreen.V10.User.UserId -> Evergreen.V12.User.UserId
migrateUserId (Evergreen.V10.User.UserId userId) =
    Evergreen.V12.User.UserId userId


migrateAscii : Evergreen.V10.Ascii.Ascii -> Evergreen.V12.Ascii.Ascii
migrateAscii (Evergreen.V10.Ascii.Ascii ascii) =
    Evergreen.V12.Ascii.Ascii ascii


migrateCell : Evergreen.V10.GridCell.Cell -> Evergreen.V12.GridCell.Cell
migrateCell (Evergreen.V10.GridCell.Cell cell) =
    Evergreen.V12.GridCell.Cell
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


migrateGrid : Evergreen.V10.Grid.Grid -> Evergreen.V12.Grid.Grid
migrateGrid (Evergreen.V10.Grid.Grid dict) =
    Dict.map (\_ v -> migrateCell v) dict |> Evergreen.V12.Grid.Grid


migrateColorIndex : Evergreen.V10.ColorIndex.ColorIndex -> Evergreen.V12.ColorIndex.ColorIndex
migrateColorIndex colorIndex =
    case colorIndex of
        Evergreen.V10.ColorIndex.Green ->
            Evergreen.V12.ColorIndex.Green

        Evergreen.V10.ColorIndex.Teal ->
            Evergreen.V12.ColorIndex.Teal

        Evergreen.V10.ColorIndex.Blue ->
            Evergreen.V12.ColorIndex.Blue

        Evergreen.V10.ColorIndex.Purple ->
            Evergreen.V12.ColorIndex.Purple

        Evergreen.V10.ColorIndex.Magenta ->
            Evergreen.V12.ColorIndex.Magenta

        Evergreen.V10.ColorIndex.Salmon ->
            Evergreen.V12.ColorIndex.Salmon

        Evergreen.V10.ColorIndex.Orange ->
            Evergreen.V12.ColorIndex.Orange

        Evergreen.V10.ColorIndex.Yellow ->
            Evergreen.V12.ColorIndex.Yellow

        Evergreen.V10.ColorIndex.Gray ->
            Evergreen.V12.ColorIndex.Gray


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
                                |> (\(Evergreen.V10.User.User { color }) -> color)
                                |> migrateColorIndex
                                |> (\a -> Evergreen.V12.User.User { color = a })
                      , hiddenUsers = SeqSet.empty
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
