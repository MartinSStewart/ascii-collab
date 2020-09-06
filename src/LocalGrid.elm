module LocalGrid exposing (LocalGrid, LocalGrid_, init, localModel, update, updateFromBackend)

import Bounds exposing (Bounds)
import Change exposing (Change(..), ClientChange(..), LocalChange(..), ServerChange(..))
import Dict exposing (Dict)
import EverySet exposing (EverySet)
import Grid exposing (Grid)
import Helper exposing (RawCellCoord)
import List.Nonempty exposing (Nonempty)
import LocalModel exposing (LocalModel)
import Time
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
    , viewBounds : Bounds CellUnit
    }


localModel : LocalModel a LocalGrid -> LocalGrid_
localModel localModel_ =
    LocalModel.localModel localModel_ |> (\(LocalGrid a) -> a)


init :
    Grid
    -> List (Dict RawCellCoord Int)
    -> List (Dict RawCellCoord Int)
    -> ( UserId, UserData )
    -> EverySet UserId
    -> List ( UserId, UserData )
    -> Bounds CellUnit
    -> LocalModel Change LocalGrid
init grid undoHistory redoHistory user hiddenUsers otherUsers viewBounds =
    LocalGrid
        { grid = grid
        , undoHistory = undoHistory
        , redoHistory = redoHistory
        , user = user
        , hiddenUsers = hiddenUsers
        , otherUsers = otherUsers
        , viewBounds = viewBounds
        }
        |> LocalModel.init


update : Time.Posix -> Change -> LocalModel Change LocalGrid -> LocalModel Change LocalGrid
update time change localModel_ =
    LocalModel.update config time change localModel_


updateFromBackend : Nonempty Change -> LocalModel Change LocalGrid -> LocalModel Change LocalGrid
updateFromBackend changes localModel_ =
    LocalModel.updateFromBackend config changes localModel_


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
            }

        LocalChange LocalRedo ->
            case model.redoHistory of
                head :: rest ->
                    { model
                        | undoHistory = Grid.undoPoint userId model.grid :: model.undoHistory
                        , redoHistory = rest
                        , grid = Grid.setUndoPoints userId head model.grid
                    }

                [] ->
                    model

        LocalChange LocalUndo ->
            case model.undoHistory of
                head :: rest ->
                    { model
                        | undoHistory = rest
                        , redoHistory = Grid.undoPoint userId model.grid :: model.redoHistory
                        , grid = Grid.setUndoPoints userId head model.grid
                    }

                [] ->
                    model

        LocalChange LocalAddUndo ->
            { model
                | redoHistory = []
                , undoHistory = Grid.undoPoint userId model.grid :: model.undoHistory
            }

        LocalChange (LocalToggleUserVisibility userId_) ->
            { model
                | hiddenUsers =
                    if userId_ == userId then
                        model.hiddenUsers

                    else if EverySet.member userId_ model.hiddenUsers then
                        EverySet.remove userId_ model.hiddenUsers

                    else
                        EverySet.insert userId_ model.hiddenUsers
            }

        ServerChange (ServerGridChange gridChange) ->
            if Bounds.contains gridChange.cellPosition model.viewBounds then
                { model | grid = Grid.addChange gridChange model.grid }

            else
                model

        ServerChange (ServerUndoPoint undoPoint) ->
            { model | grid = Grid.setUndoPoints undoPoint.userId undoPoint.undoPoints model.grid }

        ServerChange (ServerUserNew user) ->
            { model | otherUsers = user :: model.otherUsers }

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
