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
      , userIds = Dict.empty
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
            let
                userId =
                    Dict.size model.userIds |> User.userId
            in
            ( { model
                | userSessions = Set.insert ( sessionId, clientId ) model.userSessions
                , userIds = Dict.insert sessionId userId model.userIds
              }
            , Lamdera.sendToFrontend clientId (LoadingData { userId = userId, grid = model.grid })
            )

        GridChange { changes } ->
            case Dict.get sessionId model.userIds of
                Just userId ->
                    let
                        ( newGrid, changeBroadcast ) =
                            List.Nonempty.toList changes
                                |> List.foldl
                                    (\change ( grid, changeBroadcast_ ) ->
                                        ( Grid.addChange userId [ change ] grid
                                        , { cellPosition = change.cellPosition
                                          , localPosition = change.localPosition
                                          , change = change.change
                                          , changeId = Grid.changeCount change.cellPosition grid
                                          }
                                            :: changeBroadcast_
                                        )
                                    )
                                    ( model.grid, [] )
                    in
                    ( { model | grid = newGrid }
                    , case List.Nonempty.fromList changeBroadcast of
                        Just nonempty ->
                            broadcast
                                (\_ _ ->
                                    GridChangeBroadcast { changes = nonempty, userId = userId } |> Just
                                )
                                model

                        Nothing ->
                            Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


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
