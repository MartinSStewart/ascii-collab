module Backend exposing (app, init, update, updateFromFrontend)

import Bounds exposing (Bounds)
import Change exposing (ServerChange(..))
import Dict
import EverySet exposing (EverySet)
import Grid
import Lamdera exposing (ClientId, SessionId)
import List.Nonempty
import Set
import Types exposing (..)
import Units exposing (CellUnit)
import User exposing (UserData, UserId)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { grid = Grid.empty
      , userSessions = Dict.empty
      , clientData = Dict.empty
      , users = Dict.empty
      }
    , Cmd.none
    )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Lamdera.onDisconnect UserDisconnected


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        UserDisconnected sessionId clientId ->
            ( { model
                | userSessions =
                    Dict.update sessionId
                        (Maybe.map
                            (\session ->
                                { clientIds = Set.remove clientId session.clientIds
                                , userId = session.userId
                                }
                            )
                        )
                        model.userSessions
                , clientData = Dict.remove clientId model.clientData
              }
            , Cmd.none
            )


isSameUser : SessionId -> ClientId -> SessionId -> ClientId -> Bool
isSameUser s0 _ s1 _ =
    s0 == s1


isSameClient : SessionId -> ClientId -> SessionId -> ClientId -> Bool
isSameClient s0 c0 s1 c1 =
    s0 == s1 && c0 == c1


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


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

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
                        (\sessionId_ clientId_ ->
                            if isSameUser sessionId clientId sessionId_ clientId_ then
                                LocalChangeResponse changes |> Just

                            else
                                List.Nonempty.fromList serverChanges |> Maybe.map ServerChangeBroadcast
                        )
                        model
                    )

                Nothing ->
                    ( model, Cmd.none )


updateLocalChange :
    ( UserId, BackendUserData )
    -> Change.LocalChange
    -> BackendModel
    -> ( BackendModel, Maybe ServerChange )
updateLocalChange ( userId, _ ) change model =
    case change of
        Change.LocalUndo ->
            case Dict.get (User.rawId userId) model.users of
                Just undoPoint ->
                    case undoPoint.undoHistory of
                        head :: rest ->
                            ( { model
                                | users =
                                    Dict.insert (User.rawId userId)
                                        { undoPoint
                                            | undoHistory = rest
                                            , redoHistory = Grid.undoPoint userId model.grid :: undoPoint.redoHistory
                                        }
                                        model.users
                                , grid = Grid.setUndoPoints userId head model.grid
                              }
                            , ServerUndoPoint { userId = userId, undoPoints = head } |> Just
                            )

                        [] ->
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
                    case user.redoHistory of
                        head :: rest ->
                            ( { model
                                | users =
                                    Dict.insert (User.rawId userId)
                                        { user
                                            | undoHistory = Grid.undoPoint userId model.grid :: user.undoHistory
                                            , redoHistory = rest
                                        }
                                        model.users
                                , grid = Grid.setUndoPoints userId head model.grid
                              }
                            , ServerUndoPoint { userId = userId, undoPoints = head } |> Just
                            )

                        [] ->
                            ( model, Nothing )

                Nothing ->
                    ( model, Nothing )

        Change.LocalAddUndo ->
            ( updateUser
                userId
                (\user ->
                    { user
                        | redoHistory = []
                        , undoHistory = Grid.undoPoint userId model.grid :: user.undoHistory
                    }
                )
                model
            , Nothing
            )

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


requestDataUpdate : SessionId -> ClientId -> Bounds CellUnit -> BackendModel -> ( BackendModel, Cmd BackendMsg )
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
                                    Just { session | clientIds = Set.insert clientId session.clientIds }

                                Nothing ->
                                    Nothing
                        )
                        model.userSessions
                , clientData = Dict.insert clientId viewBounds model.clientData
              }
            , Lamdera.sendToFrontend clientId (LoadingData (loadingData ( userId, user )))
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
                    }
            in
            ( { model
                | userSessions =
                    Dict.insert
                        sessionId
                        { clientIds = Set.singleton clientId, userId = userId }
                        model.userSessions
                , clientData = Dict.insert clientId viewBounds model.clientData
                , users = Dict.insert (User.rawId userId) userBackendData model.users
              }
            , Cmd.batch
                [ Lamdera.sendToFrontend
                    clientId
                    (LoadingData (loadingData ( userId, userBackendData )))
                , broadcast
                    (\sessionId_ clientId_ ->
                        if isSameUser sessionId clientId sessionId_ clientId_ then
                            Nothing

                        else
                            List.Nonempty.fromElement (ServerUserNew ( userId, user )) |> ServerChangeBroadcast |> Just
                    )
                    model
                ]
            )


broadcast : (SessionId -> ClientId -> Maybe ToFrontend) -> BackendModel -> Cmd BackendMsg
broadcast msgFunc model =
    model.userSessions
        |> Dict.toList
        |> List.concatMap (\( sessionId, { clientIds } ) -> Set.toList clientIds |> List.map (Tuple.pair sessionId))
        |> List.map
            (\( sessionId, clientId ) ->
                msgFunc sessionId clientId
                    |> Maybe.map (Lamdera.sendToFrontend clientId)
                    |> Maybe.withDefault Cmd.none
            )
        |> Cmd.batch
