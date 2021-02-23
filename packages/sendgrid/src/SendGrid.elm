module SendGrid exposing
    ( Email, sendEmail, sendEmailTask, Error(..), ErrorMessage, ErrorMessage403
    , ApiKey, apiKey
    , Attachment, addAttachments, addBcc, addCc, htmlEmail, textEmail
    )

{-|


# Email

@docs Email, sendEmail, sendEmailTask, Error, ErrorMessage, ErrorMessage403


# API Key

In order to send email via SendGrid, you need an API key.
To do this signup for a SendGrid account.

Then after you've logged in, click on the settings tab, then API Keys, and finally "Create API Key".
You'll get to choose if the API key has `Full Access`, `Restricted Access`, or `Billing Access`.
For this package it's best if you select restricted access and then set `Mail Send` to `Full Access` and leave everything else as `No Access`.

Once you've done this you'll be given an API key. Store it in a password manager or something for safe keeping. _You are highly recommended to not hard code the API key into your source code!_

@docs ApiKey, apiKey


# Email content

@docs Content, textContent, htmlContent

-}

import Base64
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Email
import Email.Html
import Http
import Internal.Types
import Json.Decode as JD
import Json.Encode as JE
import List.Nonempty exposing (Nonempty)
import String.Nonempty exposing (NonemptyString)
import Task exposing (Task)


type Content
    = TextContent NonemptyString
    | HtmlContent String


type Disposition
    = AttachmentDisposition
    | Inline


encodeContent : Content -> JE.Value
encodeContent content =
    case content of
        TextContent text ->
            JE.object [ ( "type", JE.string "text/plain" ), ( "value", encodeNonemptyString text ) ]

        HtmlContent html ->
            JE.object [ ( "type", JE.string "text/html" ), ( "value", html |> JE.string ) ]


encodeEmailAddress : Email.Email -> JE.Value
encodeEmailAddress =
    Email.toString >> JE.string


encodeEmailAndName : { name : String, email : Email.Email } -> JE.Value
encodeEmailAndName emailAndName =
    JE.object [ ( "email", encodeEmailAddress emailAndName.email ), ( "name", JE.string emailAndName.name ) ]


encodePersonalization : ( Nonempty Email.Email, List Email.Email, List Email.Email ) -> JE.Value
encodePersonalization ( to, cc, bcc ) =
    let
        addName =
            List.map (\address -> { email = address, name = "" })

        ccJson =
            if List.isEmpty cc then
                []

            else
                [ ( "cc", addName cc |> JE.list encodeEmailAndName ) ]

        bccJson =
            if List.isEmpty bcc then
                []

            else
                [ ( "bcc", addName bcc |> JE.list encodeEmailAndName ) ]

        toJson =
            ( "to"
            , to
                |> List.Nonempty.map (\address -> { email = address, name = "" })
                |> encodeNonemptyList encodeEmailAndName
            )
    in
    JE.object (toJson :: ccJson ++ bccJson)


encodeNonemptyList : (a -> JE.Value) -> Nonempty a -> JE.Value
encodeNonemptyList encoder list =
    List.Nonempty.toList list |> JE.list encoder


htmlEmail :
    { subject : NonemptyString
    , content : Email.Html.Html
    , to : Nonempty Email.Email
    , nameOfSender : String
    , emailAddressOfSender : Email.Email
    }
    -> Email
htmlEmail config =
    Email
        { subject = config.subject
        , content = Internal.Types.toString config.content |> HtmlContent
        , to = config.to
        , cc = []
        , bcc = []
        , nameOfSender = config.nameOfSender
        , emailAddressOfSender = config.emailAddressOfSender
        , attachments =
            getInlineImages config.content
                |> List.map
                    (\{ name, mimeType, content } ->
                        ( name, { mimeType = mimeType, content = content, disposition = Inline } )
                    )
                |> Dict.fromList
        }


textEmail :
    { subject : NonemptyString
    , content : NonemptyString
    , to : Nonempty Email.Email
    , nameOfSender : String
    , emailAddressOfSender : Email.Email
    }
    -> Email
textEmail config =
    Email
        { subject = config.subject
        , content = TextContent config.content
        , to = config.to
        , cc = []
        , bcc = []
        , nameOfSender = config.nameOfSender
        , emailAddressOfSender = config.emailAddressOfSender
        , attachments = Dict.empty
        }


getInlineImages : Internal.Types.Html -> List { content : Bytes, mimeType : String, name : String }
getInlineImages html =
    case html of
        Internal.Types.Node _ _ children ->
            List.concatMap getInlineImages children

        Internal.Types.InlineImage inlineImage _ children ->
            inlineImage :: List.concatMap getInlineImages children

        Internal.Types.TextNode _ ->
            []


addCc : List Email.Email -> Email -> Email
addCc cc (Email email_) =
    Email { email_ | cc = email_.cc ++ cc }


addBcc : List Email.Email -> Email -> Email
addBcc bcc (Email email_) =
    Email { email_ | cc = email_.bcc ++ bcc }


addAttachments : Dict String { content : Bytes, mimeType : String } -> Email -> Email
addAttachments attachments (Email email_) =
    Email
        { email_
            | attachments =
                Dict.union
                    (Dict.map
                        (\_ value -> { content = value.content, mimeType = value.mimeType, disposition = AttachmentDisposition })
                        attachments
                    )
                    email_.attachments
        }


{-| -}
type Email
    = Email
        { subject : NonemptyString
        , content : Content
        , to : Nonempty Email.Email
        , cc : List Email.Email
        , bcc : List Email.Email
        , nameOfSender : String
        , emailAddressOfSender : Email.Email
        , attachments : Dict String Attachment
        }


type alias Attachment =
    { content : Bytes
    , mimeType : String
    , disposition : Disposition
    }


encodeDisposition : Disposition -> JE.Value
encodeDisposition disposition =
    case disposition of
        AttachmentDisposition ->
            JE.string "attachment"

        Inline ->
            JE.string "inline"


encodeAttachment : ( String, Attachment ) -> JE.Value
encodeAttachment ( filename, attachment ) =
    JE.object
        (( "content", Base64.fromBytes attachment.content |> Maybe.withDefault "" |> JE.string )
            :: ( "mimeType", JE.string attachment.mimeType )
            :: ( "filename", JE.string filename )
            :: ( "disposition", encodeDisposition attachment.disposition )
            :: (case attachment.disposition of
                    AttachmentDisposition ->
                        []

                    Inline ->
                        [ ( "content_id", JE.string filename ) ]
               )
        )


encodeNonemptyString : NonemptyString -> JE.Value
encodeNonemptyString nonemptyString =
    String.Nonempty.toString nonemptyString |> JE.string


encodeSendEmail : Email -> JE.Value
encodeSendEmail (Email { content, subject, nameOfSender, emailAddressOfSender, to, cc, bcc, attachments }) =
    let
        attachmentsList =
            Dict.toList attachments
    in
    JE.object
        (( "subject", encodeNonemptyString subject )
            :: ( "content", JE.list encodeContent [ content ] )
            :: ( "personalizations", JE.list encodePersonalization [ ( to, cc, bcc ) ] )
            :: ( "from", encodeEmailAndName { name = nameOfSender, email = emailAddressOfSender } )
            :: (case attachmentsList of
                    _ :: _ ->
                        [ ( "attachments", JE.list encodeAttachment attachmentsList ) ]

                    [] ->
                        []
               )
        )


{-| A SendGrid API key. In order to use the SendGrid API you must have one of these.
-}
type ApiKey
    = ApiKey String


{-| Create an API key from a raw string.
-}
apiKey : String -> ApiKey
apiKey apiKey_ =
    ApiKey apiKey_



--"{subject:Test
--,content:[{type:text/html,value:<div>test</div>}]
--,personalizations:[{to:[{email:m@gmail.com,name:}]}]
--,from:{email:test@test.com,name:test}}"
--
--"{subject:Test
--,content:[{type:text/html,value:<div>test</div>}]
--,personalizations:[{to:[{email:,name:}]}]
--,from:{email:test@test.test,name:test}}"
--
--"{subject:Test
--,content:[{type:text/html,value:<div>test</div>}]
--,personalizations:[{to:[{email:martinsstewart@gmail.com,name:}]}]
--,from:{email:test@test.com,name:test}}"


{-| Send an email using the SendGrid API.
-}
sendEmail : (Result Error () -> msg) -> ApiKey -> Email -> Cmd msg
sendEmail msg (ApiKey apiKey_) email_ =
    let
        _ =
            encodeSendEmail email_ |> JE.encode 0 |> Debug.log ""
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ apiKey_) ]
        , url = sendGridApiUrl
        , body = encodeSendEmail email_ |> Http.jsonBody
        , expect =
            Http.expectStringResponse msg
                (\response ->
                    case response of
                        Http.BadUrl_ url ->
                            BadUrl url |> Err

                        Http.Timeout_ ->
                            Err Timeout

                        Http.NetworkError_ ->
                            Err NetworkError

                        Http.BadStatus_ metadata body ->
                            decodeBadStatus metadata body |> Err

                        Http.GoodStatus_ _ _ ->
                            Ok ()
                )
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Send an email using the SendGrid API. This is the task version of [sendEmail](#sendEmail).
-}
sendEmailTask : ApiKey -> Email -> Task Error ()
sendEmailTask (ApiKey apiKey_) email_ =
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ apiKey_) ]
        , url = sendGridApiUrl
        , body = encodeSendEmail email_ |> Http.jsonBody
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        Http.BadUrl_ url ->
                            BadUrl url |> Err

                        Http.Timeout_ ->
                            Err Timeout

                        Http.NetworkError_ ->
                            Err NetworkError

                        Http.BadStatus_ metadata body ->
                            decodeBadStatus metadata body |> Err

                        Http.GoodStatus_ _ _ ->
                            Ok ()
                )
        , timeout = Nothing
        }


sendGridApiUrl =
    "https://api.sendgrid.com/v3/mail/send"


decodeBadStatus : Http.Metadata -> String -> Error
decodeBadStatus metadata body =
    let
        toErrorCode : (a -> Error) -> Result e a -> Error
        toErrorCode errorCode result =
            case result of
                Ok value ->
                    errorCode value

                Err _ ->
                    UnknownError { statusCode = metadata.statusCode, body = body }
    in
    case metadata.statusCode of
        400 ->
            JD.decodeString codecErrorResponse body |> toErrorCode StatusCode400

        401 ->
            JD.decodeString codecErrorResponse body |> toErrorCode StatusCode401

        403 ->
            JD.decodeString codec403ErrorResponse body |> toErrorCode StatusCode403

        413 ->
            JD.decodeString codecErrorResponse body |> toErrorCode StatusCode413

        _ ->
            UnknownError { statusCode = metadata.statusCode, body = body }


{-| Possible error codes we might get back when trying to send an email.
Some are just normal HTTP errors, others are specific to the SendGrid API.
-}
type Error
    = StatusCode400 (List ErrorMessage)
    | StatusCode401 (List ErrorMessage)
    | StatusCode403 { errors : List ErrorMessage403, id : Maybe String }
    | StatusCode413 (List ErrorMessage)
    | UnknownError { statusCode : Int, body : String }
    | NetworkError
    | Timeout
    | BadUrl String


{-| The content of a generic SendGrid error.
-}
type alias ErrorMessage =
    { field : Maybe String
    , message : String
    , errorId : Maybe String
    }


codecErrorResponse : JD.Decoder (List ErrorMessage)
codecErrorResponse =
    JD.field "errors" (JD.list codecErrorMessage)


{-| The content of a 403 status code error.
-}
type alias ErrorMessage403 =
    { message : Maybe String
    , field : Maybe String
    , help : Maybe String
    }


codec403Error : JD.Decoder ErrorMessage403
codec403Error =
    JD.map3 ErrorMessage403
        (optionalField "message" JD.string)
        (optionalField "field" JD.string)
        (optionalField "help" (JD.value |> JD.map (JE.encode 0)))


codec403ErrorResponse : JD.Decoder { errors : List ErrorMessage403, id : Maybe String }
codec403ErrorResponse =
    JD.map2 (\errors id -> { errors = errors |> Maybe.withDefault [], id = id })
        (optionalField "errors" (JD.list codec403Error))
        (optionalField "id" JD.string)


codecErrorMessage : JD.Decoder ErrorMessage
codecErrorMessage =
    JD.map3 ErrorMessage
        (optionalField "field" JD.string)
        (JD.field "message" JD.string)
        (optionalField "error_id" JD.string)


{-| Borrowed from elm-community/json-extra
-}
optionalField : String -> JD.Decoder a -> JD.Decoder (Maybe a)
optionalField fieldName decoder =
    let
        finishDecoding json =
            case JD.decodeValue (JD.field fieldName JD.value) json of
                Ok val ->
                    -- The field is present, so run the decoder on it.
                    JD.map Just (JD.field fieldName decoder)

                Err _ ->
                    -- The field was missing, which is fine!
                    JD.succeed Nothing
    in
    JD.value
        |> JD.andThen finishDecoding
