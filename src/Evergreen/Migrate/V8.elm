module Evergreen.Migrate.V8 exposing (..)

import Dict
import Evergreen.V4.Ascii
import Evergreen.V4.ColorIndex
import Evergreen.V4.Grid
import Evergreen.V4.GridCell
import Evergreen.V4.Types as Old
import Evergreen.V4.User
import Evergreen.V8.Ascii
import Evergreen.V8.ColorIndex
import Evergreen.V8.Grid
import Evergreen.V8.GridCell
import Evergreen.V8.Types as New
import Evergreen.V8.User
import Lamdera.Migrations exposing (..)
import List.Nonempty
import SeqSet


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelUnchanged


migrateUserId : Evergreen.V4.User.UserId -> Evergreen.V8.User.UserId
migrateUserId (Evergreen.V4.User.UserId userId) =
    Evergreen.V8.User.UserId userId


migrateAscii : Evergreen.V4.Ascii.Ascii -> Evergreen.V8.Ascii.Ascii
migrateAscii (Evergreen.V4.Ascii.Ascii ascii) =
    Evergreen.V8.Ascii.Ascii ascii


migrateCell : Evergreen.V4.GridCell.Cell -> Evergreen.V8.GridCell.Cell
migrateCell (Evergreen.V4.GridCell.Cell cell) =
    Evergreen.V8.GridCell.Cell
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


migrateGrid : Evergreen.V4.Grid.Grid -> Evergreen.V8.Grid.Grid
migrateGrid (Evergreen.V4.Grid.Grid dict) =
    Dict.map (\_ v -> migrateCell v) dict |> Evergreen.V8.Grid.Grid


migrateColorIndex : Evergreen.V4.ColorIndex.ColorIndex -> Evergreen.V8.ColorIndex.ColorIndex
migrateColorIndex colorIndex =
    case colorIndex of
        Evergreen.V4.ColorIndex.Green ->
            Evergreen.V8.ColorIndex.Green

        Evergreen.V4.ColorIndex.Teal ->
            Evergreen.V8.ColorIndex.Teal

        Evergreen.V4.ColorIndex.Blue ->
            Evergreen.V8.ColorIndex.Blue

        Evergreen.V4.ColorIndex.Purple ->
            Evergreen.V8.ColorIndex.Purple

        Evergreen.V4.ColorIndex.Magenta ->
            Evergreen.V8.ColorIndex.Magenta

        Evergreen.V4.ColorIndex.Salmon ->
            Evergreen.V8.ColorIndex.Salmon

        Evergreen.V4.ColorIndex.Orange ->
            Evergreen.V8.ColorIndex.Orange

        Evergreen.V4.ColorIndex.Yellow ->
            Evergreen.V8.ColorIndex.Yellow

        Evergreen.V4.ColorIndex.Gray ->
            Evergreen.V8.ColorIndex.Gray


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
                                |> (\(Evergreen.V4.User.User { color }) -> color)
                                |> migrateColorIndex
                                |> (\a -> Evergreen.V8.User.User { color = a })
                      , hiddenUsers = SeqSet.empty
                      , undoHistory = []
                      , redoHistory = []
                      , undoCurrent = Dict.empty
                      }
                    )
                )
            |> Dict.fromList
    }


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
