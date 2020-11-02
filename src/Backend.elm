module Backend exposing (app)

import BackendLogic exposing (Effect(..))
import Duration
import Env
import Lamdera exposing (ClientId, SessionId)
import SendGrid
import Time
import Types exposing (..)


app =
    Lamdera.backend
        { init = ( BackendLogic.init, Cmd.none )
        , update =
            \msg model ->
                BackendLogic.update msg model
                    |> Tuple.mapSecond (List.map effectToCmd >> Cmd.batch)
        , updateFromFrontend =
            \sessionId clientId msg model ->
                BackendLogic.updateFromFrontend sessionId clientId msg model
                    |> Tuple.mapSecond (List.map effectToCmd >> Cmd.batch)
        , subscriptions = subscriptions
        }


effectToCmd : Effect -> Cmd BackendMsg
effectToCmd effect =
    case effect of
        SendToFrontend clientId msg ->
            Lamdera.sendToFrontend clientId msg

        SendEmail msg email ->
            SendGrid.sendEmail msg Env.sendGridKey email


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onDisconnect UserDisconnected
        , Time.every (Duration.hours 3 |> Duration.inMilliseconds) NotifyAdminTimeElapsed
        ]
