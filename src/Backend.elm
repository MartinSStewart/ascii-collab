module Backend exposing (app, init, update, updateFromFrontend)

import Dict
import Grid
import Lamdera exposing (ClientId, SessionId)
import List.Nonempty
import Set
import Types exposing (..)
import User exposing (User, UserId)


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
isSameUser s0 _ s1 _ =
    s0 == s1


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

        LocalAddUndo ->
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

        LocalRename name ->
            ( { model | users = Dict.map }
            , ServerUserRename userId name |> Just
            )


isOnline : BackendModel -> SessionId -> Bool
isOnline model sessionId =
    Set.toList model.userSessions |> List.any (Tuple.first >> (==) sessionId)


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
                        , otherUsers =
                            Dict.remove sessionId model.users
                                |> Dict.filter (\k _ -> isOnline model k)
                                |> Dict.values
                        , undoHistory = []
                        , redoHistory = []
                        }
                    )
                , broadcast
                    (\sessionId_ clientId_ ->
                        if isSameUser sessionId clientId sessionId_ clientId_ then
                            Nothing

                        else
                            List.Nonempty.fromElement (ServerUserConnected user) |> ServerChangeBroadcast |> Just
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
