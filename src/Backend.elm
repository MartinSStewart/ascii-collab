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
                        a =
                            List.map identity []

                        --gridChanges : List.Nonempty.Nonempty ServerChange
                        --gridChanges =
                        --    List.Nonempty.map (localToServerChange (User.id user)) changes
                        ( newModel, serverChanges ) =
                            List.Nonempty.foldl
                                (\change ( model_, serverChanges_ ) ->
                                    case change of
                                        LocalUndo ->
                                            ( model_, serverChanges_ )

                                        LocalGridChange localChange ->
                                            ( { model_
                                                | grid =
                                                    Grid.addChange
                                                        (Grid.localChangeToChange (User.id user) localChange)
                                                        model_.grid
                                              }
                                            , ServerGridChange (Grid.localChangeToChange (User.id user) localChange)
                                                :: serverChanges_
                                            )

                                        LocalRedo ->
                                            ( model_, serverChanges_ )

                                        AddUndo ->
                                            ( model_, serverChanges_ )
                                )
                                ( model, [] )
                                changes
                                |> Tuple.mapSecond List.reverse
                    in
                    ( newModel
                    , broadcast
                        (\sessionId_ clientId_ ->
                            if sessionId == sessionId_ && clientId == clientId_ then
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


requestDataUpdate : SessionId -> ClientId -> BackendModel -> ( BackendModel, Cmd BackendMsg )
requestDataUpdate sessionId clientId model =
    case Dict.get sessionId model.users of
        Just user ->
            ( { model | userSessions = Set.insert ( sessionId, clientId ) model.userSessions }
            , Lamdera.sendToFrontend
                clientId
                (LoadingData { user = user, grid = model.grid, otherUsers = Dict.values model.users })
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
                        }
                    )
                , broadcast
                    (\sessionId_ clientId_ ->
                        if sessionId == sessionId_ && clientId == clientId_ then
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
