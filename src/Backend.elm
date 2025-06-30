module Backend exposing (app)

import BackendLogic exposing (Effect(..))
import Dict
import Duration
import Email.Html
import EmailAddress exposing (EmailAddress)
import Env
import Lamdera exposing (ClientId, SessionId)
import List.Nonempty as Nonempty
import Postmark exposing (PostmarkSend)
import String.Nonempty exposing (NonemptyString)
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

        SendEmail msg subject content to ->
            Postmark.sendEmail msg Env.postmarkKey (asciiCollabEmail subject content to)


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onDisconnect UserDisconnected
        , Time.every (BackendLogic.notifyAdminWait |> Duration.inMilliseconds) NotifyAdminTimeElapsed
        ]


asciiCollabEmail : NonemptyString -> Email.Html.Html -> EmailAddress -> PostmarkSend
asciiCollabEmail subject content to =
    { from =
        { name = "ascii-collab"
        , email =
            EmailAddress.fromString "no-reply@ascii-collab.app"
                -- This should never happen
                |> Maybe.withDefault to
        }
    , to = Nonempty.fromElement { name = "", email = to }
    , subject = subject
    , body = Postmark.BodyHtml content
    , messageStream = Postmark.BroadcastEmail
    , attachments = Dict.empty
    }
