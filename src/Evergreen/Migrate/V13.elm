module Evergreen.Migrate.V13 exposing (..)

import Dict
import Evergreen.V12.Ascii
import Evergreen.V12.ColorIndex
import Evergreen.V12.Grid
import Evergreen.V12.GridCell
import Evergreen.V12.Types as Old
import Evergreen.V12.User
import Evergreen.V13.Ascii
import Evergreen.V13.ColorIndex
import Evergreen.V13.Grid
import Evergreen.V13.GridCell
import Evergreen.V13.Types as New
import Evergreen.V13.User
import EverySet
import Lamdera.Migrations exposing (..)
import List.Nonempty


migrateUserId : Evergreen.V12.User.UserId -> Evergreen.V13.User.UserId
migrateUserId (Evergreen.V12.User.UserId userId) =
    Evergreen.V13.User.UserId userId


migrateAscii : Evergreen.V12.Ascii.Ascii -> Evergreen.V13.Ascii.Ascii
migrateAscii (Evergreen.V12.Ascii.Ascii ascii) =
    Evergreen.V13.Ascii.Ascii ascii


migrateCell : Evergreen.V12.GridCell.Cell -> Evergreen.V13.GridCell.Cell
migrateCell (Evergreen.V12.GridCell.Cell cell) =
    Evergreen.V13.GridCell.Cell
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


migrateGrid : Evergreen.V12.Grid.Grid -> Evergreen.V13.Grid.Grid
migrateGrid (Evergreen.V12.Grid.Grid dict) =
    Dict.map (\_ v -> migrateCell v) dict |> Evergreen.V13.Grid.Grid


migrateColorIndex : Evergreen.V12.ColorIndex.ColorIndex -> Evergreen.V13.ColorIndex.ColorIndex
migrateColorIndex colorIndex =
    case colorIndex of
        Evergreen.V12.ColorIndex.Green ->
            Evergreen.V13.ColorIndex.Green

        Evergreen.V12.ColorIndex.Teal ->
            Evergreen.V13.ColorIndex.Teal

        Evergreen.V12.ColorIndex.Blue ->
            Evergreen.V13.ColorIndex.Blue

        Evergreen.V12.ColorIndex.Purple ->
            Evergreen.V13.ColorIndex.Purple

        Evergreen.V12.ColorIndex.Magenta ->
            Evergreen.V13.ColorIndex.Magenta

        Evergreen.V12.ColorIndex.Salmon ->
            Evergreen.V13.ColorIndex.Salmon

        Evergreen.V12.ColorIndex.Orange ->
            Evergreen.V13.ColorIndex.Orange

        Evergreen.V12.ColorIndex.Yellow ->
            Evergreen.V13.ColorIndex.Yellow

        Evergreen.V12.ColorIndex.Gray ->
            Evergreen.V13.ColorIndex.Gray

        Evergreen.V12.ColorIndex.DarkGreen ->
            Evergreen.V13.ColorIndex.DarkGreen

        Evergreen.V12.ColorIndex.DarkTeal ->
            Evergreen.V13.ColorIndex.DarkTeal

        Evergreen.V12.ColorIndex.DarkBlue ->
            Evergreen.V13.ColorIndex.DarkBlue

        Evergreen.V12.ColorIndex.DarkPurple ->
            Evergreen.V13.ColorIndex.DarkPurple

        Evergreen.V12.ColorIndex.DarkMagenta ->
            Evergreen.V13.ColorIndex.DarkMagenta

        Evergreen.V12.ColorIndex.DarkSalmon ->
            Evergreen.V13.ColorIndex.DarkSalmon

        Evergreen.V12.ColorIndex.DarkOrange ->
            Evergreen.V13.ColorIndex.DarkOrange

        Evergreen.V12.ColorIndex.DarkYellow ->
            Evergreen.V13.ColorIndex.DarkYellow

        Evergreen.V12.ColorIndex.DarkGray ->
            Evergreen.V13.ColorIndex.DarkGray


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
                                |> (\(Evergreen.V12.User.User { color }) -> color)
                                |> migrateColorIndex
                                |> (\a -> Evergreen.V13.User.User { color = a })
                      , hiddenUsers = EverySet.empty
                      , hiddenForAll = False
                      , undoHistory = []
                      , redoHistory = []
                      , undoCurrent = Dict.empty
                      }
                    )
                )
            |> Dict.fromList
    , usersHiddenRecently = []
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
