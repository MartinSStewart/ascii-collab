module Backend exposing (app)

import BackendLogic exposing (Effect(..))
import Duration
import Env
import Lamdera exposing (ClientId, SessionId)
import SendGrid
import Task
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
                ( model, Time.now |> Task.perform (UpdateFromFrontend sessionId clientId msg) )
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
        , Time.every (BackendLogic.notifyAdminWait |> Duration.inMilliseconds) NotifyAdminTimeElapsed
        ]
