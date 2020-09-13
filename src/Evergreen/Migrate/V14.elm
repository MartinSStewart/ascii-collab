module Evergreen.Migrate.V14 exposing (..)

import Dict
import Evergreen.V13.Ascii
import Evergreen.V13.ColorIndex
import Evergreen.V13.Grid
import Evergreen.V13.GridCell
import Evergreen.V13.Helper
import Evergreen.V13.Types as Old
import Evergreen.V13.User
import Evergreen.V14.Ascii
import Evergreen.V14.ColorIndex
import Evergreen.V14.Grid
import Evergreen.V14.GridCell
import Evergreen.V14.Helper
import Evergreen.V14.Types as New
import Evergreen.V14.User
import EverySet
import Lamdera.Migrations exposing (..)
import List.Nonempty
import Quantity exposing (Quantity(..))


migrateUserId : Evergreen.V13.User.UserId -> Evergreen.V14.User.UserId
migrateUserId (Evergreen.V13.User.UserId userId) =
    Evergreen.V14.User.UserId userId


migrateAscii : Evergreen.V13.Ascii.Ascii -> Evergreen.V14.Ascii.Ascii
migrateAscii (Evergreen.V13.Ascii.Ascii ascii) =
    Evergreen.V14.Ascii.Ascii ascii


migrateCell : Evergreen.V13.GridCell.Cell -> Evergreen.V14.GridCell.Cell
migrateCell (Evergreen.V13.GridCell.Cell cell) =
    Evergreen.V14.GridCell.Cell
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


migrateGrid : Evergreen.V13.Grid.Grid -> Evergreen.V14.Grid.Grid
migrateGrid (Evergreen.V13.Grid.Grid dict) =
    Dict.map (\_ v -> migrateCell v) dict |> Evergreen.V14.Grid.Grid


migrateQuantity : Quantity number a -> Quantity number b
migrateQuantity (Quantity quantity) =
    Quantity quantity


migrateCoord : Evergreen.V13.Helper.Coord a -> Evergreen.V14.Helper.Coord b
migrateCoord ( x, y ) =
    ( migrateQuantity x, migrateQuantity y )


migrateColorIndex : Evergreen.V13.ColorIndex.ColorIndex -> Evergreen.V14.ColorIndex.ColorIndex
migrateColorIndex colorIndex =
    case colorIndex of
        Evergreen.V13.ColorIndex.Green ->
            Evergreen.V14.ColorIndex.Green

        Evergreen.V13.ColorIndex.Teal ->
            Evergreen.V14.ColorIndex.Teal

        Evergreen.V13.ColorIndex.Blue ->
            Evergreen.V14.ColorIndex.Blue

        Evergreen.V13.ColorIndex.Purple ->
            Evergreen.V14.ColorIndex.Purple

        Evergreen.V13.ColorIndex.Magenta ->
            Evergreen.V14.ColorIndex.Magenta

        Evergreen.V13.ColorIndex.Salmon ->
            Evergreen.V14.ColorIndex.Salmon

        Evergreen.V13.ColorIndex.Orange ->
            Evergreen.V14.ColorIndex.Orange

        Evergreen.V13.ColorIndex.Yellow ->
            Evergreen.V14.ColorIndex.Yellow

        Evergreen.V13.ColorIndex.Gray ->
            Evergreen.V14.ColorIndex.Gray

        Evergreen.V13.ColorIndex.DarkGreen ->
            Evergreen.V14.ColorIndex.DarkGreen

        Evergreen.V13.ColorIndex.DarkTeal ->
            Evergreen.V14.ColorIndex.DarkTeal

        Evergreen.V13.ColorIndex.DarkBlue ->
            Evergreen.V14.ColorIndex.DarkBlue

        Evergreen.V13.ColorIndex.DarkPurple ->
            Evergreen.V14.ColorIndex.DarkPurple

        Evergreen.V13.ColorIndex.DarkMagenta ->
            Evergreen.V14.ColorIndex.DarkMagenta

        Evergreen.V13.ColorIndex.DarkSalmon ->
            Evergreen.V14.ColorIndex.DarkSalmon

        Evergreen.V13.ColorIndex.DarkOrange ->
            Evergreen.V14.ColorIndex.DarkOrange

        Evergreen.V13.ColorIndex.DarkYellow ->
            Evergreen.V14.ColorIndex.DarkYellow

        Evergreen.V13.ColorIndex.DarkGray ->
            Evergreen.V14.ColorIndex.DarkGray


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
                                |> (\(Evergreen.V13.User.User { color }) -> color)
                                |> migrateColorIndex
                                |> (\a -> Evergreen.V14.User.User { color = a })
                      , hiddenUsers = EverySet.empty
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
