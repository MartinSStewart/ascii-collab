module Evergreen.Migrate.V4 exposing (..)

import Dict
import Evergreen.V3.Ascii
import Evergreen.V3.ColorIndex
import Evergreen.V3.Grid
import Evergreen.V3.GridCell
import Evergreen.V3.Types as Old
import Evergreen.V3.User
import Evergreen.V4.Ascii
import Evergreen.V4.ColorIndex
import Evergreen.V4.Grid
import Evergreen.V4.GridCell
import Evergreen.V4.Types as New
import Evergreen.V4.User
import EverySet
import Lamdera.Migrations exposing (..)
import List.Extra as List
import List.Nonempty
import Set


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelUnchanged


migrateUserId : Evergreen.V3.User.UserId -> Evergreen.V4.User.UserId
migrateUserId (Evergreen.V3.User.UserId userId) =
    Evergreen.V4.User.UserId userId


migrateAscii : Evergreen.V3.Ascii.Ascii -> Evergreen.V4.Ascii.Ascii
migrateAscii (Evergreen.V3.Ascii.Ascii ascii) =
    Evergreen.V4.Ascii.Ascii ascii


migrateCell : Evergreen.V3.GridCell.Cell -> Evergreen.V4.GridCell.Cell
migrateCell (Evergreen.V3.GridCell.Cell cell) =
    Evergreen.V4.GridCell.Cell
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


migrateGrid : Evergreen.V3.Grid.Grid -> Evergreen.V4.Grid.Grid
migrateGrid (Evergreen.V3.Grid.Grid dict) =
    Dict.map (\_ v -> migrateCell v) dict |> Evergreen.V4.Grid.Grid


migrateColorIndex : Evergreen.V3.ColorIndex.ColorIndex -> Evergreen.V4.ColorIndex.ColorIndex
migrateColorIndex colorIndex =
    case colorIndex of
        Evergreen.V3.ColorIndex.Green ->
            Evergreen.V4.ColorIndex.Green

        Evergreen.V3.ColorIndex.Teal ->
            Evergreen.V4.ColorIndex.Teal

        Evergreen.V3.ColorIndex.Blue ->
            Evergreen.V4.ColorIndex.Blue

        Evergreen.V3.ColorIndex.Purple ->
            Evergreen.V4.ColorIndex.Purple

        Evergreen.V3.ColorIndex.Magenta ->
            Evergreen.V4.ColorIndex.Magenta

        Evergreen.V3.ColorIndex.Salmon ->
            Evergreen.V4.ColorIndex.Salmon

        Evergreen.V3.ColorIndex.Orange ->
            Evergreen.V4.ColorIndex.Orange

        Evergreen.V3.ColorIndex.Yellow ->
            Evergreen.V4.ColorIndex.Yellow

        Evergreen.V3.ColorIndex.Gray ->
            Evergreen.V4.ColorIndex.Gray


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ( { grid = migrateGrid old.grid
      , undoPoints = old.undoPoints
      , userSessions =
            old.userSessions
                |> Set.toList
                |> List.gatherEqualsBy Tuple.first
                |> List.filterMap
                    (\( ( sessionId, _ ), _ ) ->
                        case Dict.get sessionId old.users of
                            Just (Evergreen.V3.User.User { id }) ->
                                Just
                                    ( sessionId
                                    , { clientIds = Set.empty
                                      , userId = migrateUserId id
                                      }
                                    )

                            Nothing ->
                                Nothing
                    )
                |> Dict.fromList
      , users =
            Dict.toList old.users
                |> List.map
                    (\( _, Evergreen.V3.User.User { id, color } ) ->
                        let
                            (Evergreen.V3.User.UserId rawId) =
                                id
                        in
                        ( rawId
                        , { userData = Evergreen.V4.User.User { color = migrateColorIndex color }
                          , hiddenUsers = EverySet.empty
                          }
                        )
                    )
                |> Dict.fromList
      }
    , Cmd.none
    )
        |> ModelMigrated


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
