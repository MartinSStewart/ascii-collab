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
            ( { model | grid = Grid.addChange (User.fromSessionId sessionId) (List.Nonempty.toList changes) model.grid }
            , broadcast
                (GridChangeBroadcast
                    { changes =
                        List.Nonempty.map
                            (\{ cellPosition, localPosition, change } ->
                                { cellPosition = cellPosition
                                , localPosition = localPosition
                                , change = change
                                , changeId = Grid.changeCount cellPosition model.grid
                                , userId = User.fromSessionId sessionId
                                }
                            )
                            changes
                    , user = User.fromSessionId sessionId
                    }
                )
                model
            )


broadcast : ToFrontend -> BackendModel -> Cmd BackendMsg
broadcast msg model =
    Set.toList model.users |> List.map (\( _, clientId ) -> Lamdera.sendToFrontend clientId msg) |> Cmd.batch
