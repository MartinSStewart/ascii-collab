module Backend exposing (app, init, update, updateFromFrontend)

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
      , users = Set.empty
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
            ( { model | users = Set.remove ( sessionId, clientId ) model.users }, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        RequestData ->
            ( { model | users = Set.insert ( sessionId, clientId ) model.users }
            , Lamdera.sendToFrontend clientId (LoadingData { userId = User.fromSessionId sessionId, grid = model.grid })
            )

        GridChange { changes } ->
            let
                ( newGrid, changeBroadcast ) =
                    List.Nonempty.toList changes
                        |> List.foldl
                            (\change ( grid, changeBroadcast_ ) ->
                                ( Grid.addChange (User.fromSessionId sessionId) [ change ] grid
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
                        (\sessionId_ clientId_ ->
                            if sessionId_ == sessionId && clientId_ == clientId then
                                Nothing

                            else
                                GridChangeBroadcast { changes = nonempty, user = User.fromSessionId sessionId } |> Just
                        )
                        model

                Nothing ->
                    Cmd.none
            )


broadcast : (SessionId -> ClientId -> Maybe ToFrontend) -> BackendModel -> Cmd BackendMsg
broadcast msgFunc model =
    Set.toList model.users
        |> List.map
            (\( sessionId, clientId ) ->
                msgFunc sessionId clientId
                    |> Maybe.map (Lamdera.sendToFrontend clientId)
                    |> Maybe.withDefault Cmd.none
            )
        |> Cmd.batch
