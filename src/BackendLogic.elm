module BackendLogic exposing (Effect(..), init, update, updateFromFrontend)

import Bounds exposing (Bounds)
import Change exposing (ClientChange(..), ServerChange(..))
import Dict
import EverySet exposing (EverySet)
import Grid
import List.Nonempty
import Types exposing (..)
import Undo
import Units exposing (CellUnit)
import User exposing (UserData, UserId)


type Effect
    = Effect ClientId ToFrontend


init : BackendModel
init =
    { grid = Grid.empty, userSessions = Dict.empty, users = Dict.empty }


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        UserDisconnected sessionId clientId ->
            ( { model
                | userSessions =
                    Dict.update sessionId
                        (Maybe.map
                            (\session ->
                                { clientIds = Dict.remove clientId session.clientIds
                                , userId = session.userId
                                }
                            )
                        )
                        model.userSessions
              }
            , Cmd.none
            )


getUserFromSessionId : SessionId -> BackendModel -> Maybe ( UserId, BackendUserData )
getUserFromSessionId sessionId model =
    case Dict.get sessionId model.userSessions of
        Just { userId } ->
            case Dict.get (User.rawId userId) model.users of
                Just user ->
                    Just ( userId, user )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, List Effect )
updateFromFrontend sessionId clientId msg model =
    case msg of
        RequestData requestData ->
            requestDataUpdate sessionId clientId requestData model

        GridChange changes ->
            case getUserFromSessionId sessionId model of
                Just userIdAndUser ->
                    let
                        ( newModel, serverChanges ) =
                            List.Nonempty.foldl
                                (\change ( model_, serverChanges_ ) ->
                                    updateLocalChange userIdAndUser change model_
                                        |> Tuple.mapSecond (\serverChange -> serverChange :: serverChanges_)
                                )
                                ( model, [] )
                                changes
                                |> Tuple.mapSecond (List.filterMap identity >> List.reverse)
                    in
                    ( newModel
                    , broadcast
                        (\sessionId_ _ ->
                            if sessionId == sessionId_ then
                                List.Nonempty.map Change.LocalChange changes |> ChangeBroadcast |> Just

                            else
                                List.map Change.ServerChange serverChanges
                                    |> List.Nonempty.fromList
                                    |> Maybe.map ChangeBroadcast
                        )
                        model
                    )

                Nothing ->
                    ( model, [] )

        ChangeViewBounds bounds ->
            case
                Dict.get sessionId model.userSessions
                    |> Maybe.andThen (\{ clientIds } -> Dict.get clientId clientIds)
            of
                Just oldBounds ->
                    let
                        newCells =
                            Bounds.coordRangeFold
                                (\coord newCells_ ->
                                    if Bounds.contains coord oldBounds then
                                        newCells_

                                    else
                                        case Grid.getCell coord model.grid of
                                            Just cell ->
                                                ( coord, cell ) :: newCells_

                                            Nothing ->
                                                newCells_
                                )
                                identity
                                bounds
                                []
                    in
                    ( { model
                        | userSessions =
                            Dict.update
                                sessionId
                                (Maybe.map
                                    (\session ->
                                        { session
                                            | clientIds = Dict.update clientId (\_ -> Just bounds) session.clientIds
                                        }
                                    )
                                )
                                model.userSessions
                      }
                    , ViewBoundsChange bounds newCells
                        |> Change.ClientChange
                        |> List.Nonempty.fromElement
                        |> ChangeBroadcast
                        |> Effect clientId
                        |> List.singleton
                    )

                Nothing ->
                    ( model, [] )


updateLocalChange :
    ( UserId, BackendUserData )
    -> Change.LocalChange
    -> BackendModel
    -> ( BackendModel, Maybe ServerChange )
updateLocalChange ( userId, _ ) change model =
    case change of
        Change.LocalUndo ->
            case Dict.get (User.rawId userId) model.users of
                Just user ->
                    case Undo.undo user of
                        Just newUser ->
                            let
                                undoMoveAmount =
                                    Dict.map (\_ a -> -a) user.undoCurrent
                            in
                            ( { model
                                | users = Dict.insert (User.rawId userId) newUser model.users
                                , grid = Grid.moveUndoPoint userId undoMoveAmount model.grid
                              }
                            , ServerUndoPoint { userId = userId, undoPoints = undoMoveAmount } |> Just
                            )

                        Nothing ->
                            ( model, Nothing )

                Nothing ->
                    ( model, Nothing )

        Change.LocalGridChange localChange ->
            ( { model
                | grid =
                    Grid.addChange
                        (Grid.localChangeToChange userId localChange)
                        model.grid
              }
            , ServerGridChange (Grid.localChangeToChange userId localChange) |> Just
            )

        Change.LocalRedo ->
            case Dict.get (User.rawId userId) model.users of
                Just user ->
                    case Undo.redo user of
                        Just newUser ->
                            let
                                undoMoveAmount =
                                    newUser.undoCurrent
                            in
                            ( { model
                                | users = Dict.insert (User.rawId userId) newUser model.users
                                , grid = Grid.moveUndoPoint userId undoMoveAmount model.grid
                              }
                            , ServerUndoPoint { userId = userId, undoPoints = undoMoveAmount } |> Just
                            )

                        Nothing ->
                            ( model, Nothing )

                Nothing ->
                    ( model, Nothing )

        Change.LocalAddUndo ->
            ( updateUser userId Undo.add model, Nothing )

        Change.LocalToggleUserVisibility userId_ ->
            ( if userId == userId_ then
                model

              else
                updateUser
                    userId
                    (\user ->
                        { user
                            | hiddenUsers =
                                if EverySet.member userId_ user.hiddenUsers then
                                    EverySet.remove userId_ user.hiddenUsers

                                else
                                    EverySet.insert userId_ user.hiddenUsers
                        }
                    )
                    model
            , Nothing
            )


updateUser : UserId -> (BackendUserData -> BackendUserData) -> BackendModel -> BackendModel
updateUser userId updateUserFunc model =
    { model | users = Dict.update (User.rawId userId) (Maybe.map updateUserFunc) model.users }


requestDataUpdate : SessionId -> ClientId -> Bounds CellUnit -> BackendModel -> ( BackendModel, List Effect )
requestDataUpdate sessionId clientId viewBounds model =
    let
        loadingData ( userId, user ) =
            { user = ( userId, user.userData )
            , grid = Grid.region viewBounds model.grid
            , hiddenUsers = user.hiddenUsers
            , otherUsers =
                Dict.toList model.users
                    |> List.map (Tuple.mapBoth User.userId .userData)
                    |> List.filter (Tuple.first >> (/=) userId)
            , undoHistory = user.undoHistory
            , redoHistory = user.redoHistory
            , viewBounds = viewBounds
            }
    in
    case getUserFromSessionId sessionId model of
        Just ( userId, user ) ->
            ( { model
                | userSessions =
                    Dict.update sessionId
                        (\maybeSession ->
                            case maybeSession of
                                Just session ->
                                    Just { session | clientIds = Dict.insert clientId viewBounds session.clientIds }

                                Nothing ->
                                    Nothing
                        )
                        model.userSessions
              }
            , [ Effect clientId (LoadingData (loadingData ( userId, user ))) ]
            )

        Nothing ->
            let
                ( userId, user ) =
                    Dict.size model.users |> User.newUser

                userBackendData =
                    { userData = user
                    , hiddenUsers = EverySet.empty
                    , undoHistory = []
                    , redoHistory = []
                    , undoCurrent = Dict.empty
                    }
            in
            ( { model
                | userSessions =
                    Dict.insert
                        sessionId
                        { clientIds = Dict.singleton clientId viewBounds, userId = userId }
                        model.userSessions
                , users = Dict.insert (User.rawId userId) userBackendData model.users
              }
            , Effect
                clientId
                (LoadingData (loadingData ( userId, userBackendData )))
                :: broadcast
                    (\sessionId_ _ ->
                        if sessionId == sessionId_ then
                            Nothing

                        else
                            ServerUserNew ( userId, user )
                                |> Change.ServerChange
                                |> List.Nonempty.fromElement
                                |> ChangeBroadcast
                                |> Just
                    )
                    model
            )


broadcast : (SessionId -> ClientId -> Maybe ToFrontend) -> BackendModel -> List Effect
broadcast msgFunc model =
    model.userSessions
        |> Dict.toList
        |> List.concatMap (\( sessionId, { clientIds } ) -> Dict.keys clientIds |> List.map (Tuple.pair sessionId))
        |> List.filterMap (\( sessionId, clientId ) -> msgFunc sessionId clientId |> Maybe.map (Effect clientId))
