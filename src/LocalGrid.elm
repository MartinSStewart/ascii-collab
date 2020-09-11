module LocalGrid exposing (LocalGrid, LocalGrid_, incrementUndoCurrent, init, localModel, update, updateFromBackend)

import Bounds exposing (Bounds)
import Change exposing (Change(..), ClientChange(..), LocalChange(..), ServerChange(..))
import Dict exposing (Dict)
import EverySet exposing (EverySet)
import Grid exposing (Grid)
import Helper exposing (Coord, RawCellCoord)
import List.Nonempty exposing (Nonempty)
import LocalModel exposing (LocalModel)
import Time
import Undo
import Units exposing (CellUnit)
import User exposing (UserData, UserId)


type LocalGrid
    = LocalGrid LocalGrid_


type alias LocalGrid_ =
    { grid : Grid
    , undoHistory : List (Dict RawCellCoord Int)
    , redoHistory : List (Dict RawCellCoord Int)
    , user : ( UserId, UserData )
    , otherUsers : List ( UserId, UserData )
    , hiddenUsers : EverySet UserId
    , adminHiddenUsers : EverySet UserId
    , viewBounds : Bounds CellUnit
    , undoCurrent : Dict RawCellCoord Int
    }


localModel : LocalModel a LocalGrid -> LocalGrid_
localModel localModel_ =
    LocalModel.localModel localModel_ |> (\(LocalGrid a) -> a)


init :
    { user : ( UserId, UserData )
    , grid : Grid
    , otherUsers : List ( UserId, UserData )
    , hiddenUsers : EverySet UserId
    , adminHiddenUsers : EverySet UserId
    , undoHistory : List (Dict RawCellCoord Int)
    , redoHistory : List (Dict RawCellCoord Int)
    , undoCurrent : Dict RawCellCoord Int
    , viewBounds : Bounds CellUnit
    }
    -> LocalModel Change LocalGrid
init { grid, undoHistory, redoHistory, undoCurrent, user, hiddenUsers, adminHiddenUsers, otherUsers, viewBounds } =
    LocalGrid
        { grid = grid
        , undoHistory = undoHistory
        , redoHistory = redoHistory
        , user = user
        , hiddenUsers = hiddenUsers
        , adminHiddenUsers = adminHiddenUsers
        , otherUsers = otherUsers
        , viewBounds = viewBounds
        , undoCurrent = undoCurrent
        }
        |> LocalModel.init


update : Time.Posix -> Change -> LocalModel Change LocalGrid -> LocalModel Change LocalGrid
update time change localModel_ =
    LocalModel.update config time change localModel_


updateFromBackend : Nonempty Change -> LocalModel Change LocalGrid -> LocalModel Change LocalGrid
updateFromBackend changes localModel_ =
    LocalModel.updateFromBackend config changes localModel_


incrementUndoCurrent : { a | cellPosition : Coord units } -> Dict RawCellCoord Int -> Dict RawCellCoord Int
incrementUndoCurrent gridChange undoCurrent =
    Dict.update
        (Helper.toRawCoord gridChange.cellPosition)
        (Maybe.withDefault 0 >> (+) 1 >> Just)
        undoCurrent


update_ : Change -> LocalGrid_ -> LocalGrid_
update_ msg model =
    let
        userId =
            Tuple.first model.user
    in
    case msg of
        LocalChange (LocalGridChange gridChange) ->
            { model
                | redoHistory = []
                , grid =
                    if Bounds.contains gridChange.cellPosition model.viewBounds then
                        Grid.addChange (Grid.localChangeToChange userId gridChange) model.grid

                    else
                        model.grid
                , undoCurrent = incrementUndoCurrent gridChange model.undoCurrent
            }

        LocalChange LocalRedo ->
            case Undo.redo model of
                Just newModel ->
                    { newModel | grid = Grid.moveUndoPoint userId newModel.undoCurrent model.grid }

                Nothing ->
                    model

        LocalChange LocalUndo ->
            case Undo.undo model of
                Just newModel ->
                    { newModel | grid = Grid.moveUndoPoint userId (Dict.map (\_ a -> -a) model.undoCurrent) model.grid }

                Nothing ->
                    model

        LocalChange LocalAddUndo ->
            Undo.add model

        LocalChange (LocalToggleUserVisibility userId_) ->
            { model
                | hiddenUsers =
                    if userId_ == userId then
                        model.hiddenUsers

                    else
                        Helper.toggleSet userId_ model.hiddenUsers
            }

        LocalChange (LocalToggleUserVisibilityForAll hideUserId) ->
            { model | adminHiddenUsers = Helper.toggleSet hideUserId model.adminHiddenUsers }

        ServerChange (ServerGridChange gridChange) ->
            if Bounds.contains gridChange.cellPosition model.viewBounds then
                { model | grid = Grid.addChange gridChange model.grid }

            else
                model

        ServerChange (ServerUndoPoint undoPoint) ->
            { model | grid = Grid.moveUndoPoint undoPoint.userId undoPoint.undoPoints model.grid }

        ServerChange (ServerUserNew user) ->
            { model | otherUsers = user :: model.otherUsers }

        ServerChange (ServerToggleUserVisibilityForAll hideUserId) ->
            { model | adminHiddenUsers = Helper.toggleSet hideUserId model.adminHiddenUsers }

        ClientChange (ViewBoundsChange bounds newCells) ->
            { model
                | grid =
                    Grid.allCellsDict model.grid
                        |> Dict.filter (\coord _ -> Bounds.contains (Helper.fromRawCoord coord) bounds)
                        |> Dict.union (List.map (Tuple.mapFirst Helper.toRawCoord) newCells |> Dict.fromList)
                        |> Grid.from
                , viewBounds = bounds
            }


config : LocalModel.Config Change LocalGrid
config =
    { msgEqual =
        \msg0 msg1 ->
            case ( msg0, msg1 ) of
                ( ClientChange (ViewBoundsChange bounds0 _), ClientChange (ViewBoundsChange bounds1 _) ) ->
                    bounds0 == bounds1

                _ ->
                    msg0 == msg1
    , update = \msg (LocalGrid model) -> update_ msg model |> LocalGrid
    }
