module Backend exposing (app, init, update, updateFromFrontend)

import Dict
import Grid
import Html
import Lamdera exposing (ClientId, SessionId)
import Set
import Types exposing (..)
import User exposing (UserId)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { grid = Grid.empty_, users = Set.empty, changeCount = 0 }, Cmd.none )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        RequestData ->
            ( model, Lamdera.sendToFrontend clientId (LoadingData { userId = User.fromSessionId sessionId, grid = model.grid }) )

        GridChange { changes } ->
            ( { model | changeCount = model.changeCount + 1 }
            , broadcast
                (GridChangeBroadcast { changes = changes, changeId = model.changeCount, user = User.fromSessionId sessionId })
                model
            )


broadcast : ToFrontend -> BackendModel -> Cmd BackendMsg
broadcast msg model =
    Set.toList model.users |> List.map (\( _, clientId ) -> Lamdera.sendToFrontend clientId msg) |> Cmd.batch
