module Backend exposing (app)

import BackendLogic exposing (Effect(..))
import Duration
import Lamdera exposing (ClientId, SessionId)
import Time
import Types exposing (..)


app =
    Lamdera.backend
        { init = ( BackendLogic.init, Cmd.none )
        , update = BackendLogic.update
        , updateFromFrontend =
            \sessionId clientId msg model ->
                BackendLogic.updateFromFrontend sessionId clientId msg model
                    |> Tuple.mapSecond
                        (List.map (\(Effect clientId_ effectMsg) -> Lamdera.sendToFrontend clientId_ effectMsg)
                            >> Cmd.batch
                        )
        , subscriptions = subscriptions
        }


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onDisconnect UserDisconnected
        , Time.every (Duration.hours 3 |> Duration.inMilliseconds) NotifyAdminTimeElapsed
        ]
