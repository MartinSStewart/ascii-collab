module Backend exposing (app, init, update, updateFromFrontend)

import Dict
import Grid
import Lamdera exposing (ClientId, SessionId)
import List.Nonempty
import Set
import Types exposing (..)
import User exposing (UserId)


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
      , userSessions = Set.empty
      , users = Dict.empty
      , undoPoints = Dict.empty
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
            ( { model | userSessions = Set.remove ( sessionId, clientId ) model.userSessions }, Cmd.none )


isSameUser : SessionId -> ClientId -> SessionId -> ClientId -> Bool
isSameUser s0 c0 s1 c1 =
    s0 == s1



--s0 == s1


isSameClient : SessionId -> ClientId -> SessionId -> ClientId -> Bool
isSameClient s0 c0 s1 c1 =
    s0 == s1 && c0 == c1


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        RequestData ->
            requestDataUpdate sessionId clientId model

        GridChange changes ->
            case Dict.get sessionId model.users of
                Just user ->
                    let
                        userId =
                            User.id user

                        ( newModel, serverChanges ) =
                            List.Nonempty.foldl
                                (\change ( model_, serverChanges_ ) ->
                                    updateLocalChange userId change model_
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

        UserRename name ->
            case Dict.get sessionId model.users |> Maybe.andThen (User.withName name) of
                Just userRenamed ->
                    ( { model | users = Dict.insert sessionId userRenamed model.users }
                    , broadcast (\_ _ -> UserModifiedBroadcast userRenamed |> Just) model
                    )

                Nothing ->
                    ( model, Cmd.none )


updateLocalChange : UserId -> LocalChange -> BackendModel -> ( BackendModel, Maybe ServerChange )
updateLocalChange userId change model =
    case change of
        LocalUndo ->
            case Dict.get (User.rawId userId) model.undoPoints of
                Just undoPoint ->
                    case undoPoint.undoHistory of
                        head :: rest ->
                            ( { model
                                | undoPoints =
                                    Dict.insert (User.rawId userId)
                                        { undoPoint
                                            | undoHistory = rest
                                            , redoHistory = Grid.undoPoint userId model.grid :: undoPoint.redoHistory
                                        }
                                        model.undoPoints
                                , grid = Grid.setUndoPoints userId head model.grid
                              }
                            , ServerUndoPoint { userId = userId, undoPoints = head } |> Just
                            )

                        [] ->
                            ( model, Nothing )

                Nothing ->
                    ( model, Nothing )

        LocalGridChange localChange ->
            ( { model
                | grid =
                    Grid.addChange
                        (Grid.localChangeToChange userId localChange)
                        model.grid
              }
            , ServerGridChange (Grid.localChangeToChange userId localChange) |> Just
            )

        LocalRedo ->
            case Dict.get (User.rawId userId) model.undoPoints of
                Just undoPoint ->
                    case undoPoint.redoHistory of
                        head :: rest ->
                            ( { model
                                | undoPoints =
                                    Dict.insert (User.rawId userId)
                                        { undoPoint
                                            | undoHistory = Grid.undoPoint userId model.grid :: undoPoint.undoHistory
                                            , redoHistory = rest
                                        }
                                        model.undoPoints
                                , grid = Grid.setUndoPoints userId head model.grid
                              }
                            , ServerUndoPoint { userId = userId, undoPoints = head } |> Just
                            )

                        [] ->
                            ( model, Nothing )

                Nothing ->
                    ( model, Nothing )

        AddUndo ->
            ( { model
                | undoPoints =
                    Dict.update
                        (User.rawId userId)
                        (\maybeValue ->
                            (case maybeValue of
                                Just value ->
                                    { value
                                        | redoHistory = []
                                        , undoHistory = Grid.undoPoint userId model.grid :: value.undoHistory
                                    }

                                Nothing ->
                                    { redoHistory = []
                                    , undoHistory = [ Grid.undoPoint userId model.grid ]
                                    }
                            )
                                |> Just
                        )
                        model.undoPoints
              }
            , Nothing
            )


requestDataUpdate : SessionId -> ClientId -> BackendModel -> ( BackendModel, Cmd BackendMsg )
requestDataUpdate sessionId clientId model =
    case Dict.get sessionId model.users of
        Just user ->
            let
                undoPoints =
                    Dict.get (User.id user |> User.rawId) model.undoPoints
            in
            ( { model | userSessions = Set.insert ( sessionId, clientId ) model.userSessions }
            , Lamdera.sendToFrontend
                clientId
                (LoadingData
                    { user = user
                    , grid = model.grid
                    , otherUsers = Dict.values model.users
                    , undoHistory = Maybe.map .undoHistory undoPoints |> Maybe.withDefault []
                    , redoHistory = Maybe.map .redoHistory undoPoints |> Maybe.withDefault []
                    }
                )
            )

        Nothing ->
            let
                user =
                    Dict.size model.users |> User.fromIndex
            in
            ( { model
                | userSessions = Set.insert ( sessionId, clientId ) model.userSessions
                , users = Dict.insert sessionId user model.users
              }
            , Cmd.batch
                [ Lamdera.sendToFrontend
                    clientId
                    (LoadingData
                        { user = user
                        , grid = model.grid
                        , otherUsers = Dict.remove sessionId model.users |> Dict.values
                        , undoHistory = []
                        , redoHistory = []
                        }
                    )
                , broadcast
                    (\sessionId_ clientId_ ->
                        if isSameUser sessionId clientId sessionId_ clientId_ then
                            Nothing

                        else
                            NewUserBroadcast user |> Just
                    )
                    model
                ]
            )


broadcast : (SessionId -> ClientId -> Maybe ToFrontend) -> BackendModel -> Cmd BackendMsg
broadcast msgFunc model =
    Set.toList model.userSessions
        |> List.map
            (\( sessionId, clientId ) ->
                msgFunc sessionId clientId
                    |> Maybe.map (Lamdera.sendToFrontend clientId)
                    |> Maybe.withDefault Cmd.none
            )
        |> Cmd.batch
