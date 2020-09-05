module Backend exposing (app)

import BackendLogic exposing (Effect(..))
import Lamdera exposing (ClientId, SessionId)
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
    Lamdera.onDisconnect UserDisconnected
