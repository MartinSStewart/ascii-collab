module Postmark exposing
    ( ApiKey
    , MessageStream(..)
    , PostmarkEmailBody(..)
    , PostmarkSend
    , PostmarkSendResponse
    , SendEmailError(..)
    , SendEmailsError(..)
    , apiKey
    , sendEmail
    , sendEmailTask
    , sendEmails
    , sendEmailsTask
    )

import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Email.Html2
import EmailAddress2 exposing (EmailAddress)
import Http
import Internal
import Json.Decode as D
import Json.Encode as E
import List.Nonempty exposing (Nonempty(..))
import String.Nonempty exposing (NonemptyString)
import Task exposing (Task)
import VendoredBase64


endpoint : String
endpoint =
    "https://api.postmarkapp.com"


{-| A SendGrid API key. In order to send an email you must have one of these (see the readme for how to get one).
-}
type ApiKey
    = ApiKey String


{-| Create an API key from a raw string (see the readme for how to get one).
-}
apiKey : String -> ApiKey
apiKey apiKey_ =
    ApiKey apiKey_


type PostmarkEmailBody
    = BodyHtml Email.Html2.Html
    | BodyText String
    | BodyBoth Email.Html2.Html String



-- Plain send


type alias PostmarkSend =
    { from : { name : String, email : EmailAddress }
    , to : Nonempty { name : String, email : EmailAddress }
    , subject : NonemptyString
    , body : PostmarkEmailBody
    , messageStream : MessageStream
    , attachments : Dict String { content : String, mimeType : String }
    }


{-| What type of email are you sending? `TransactionalEmail` is for high priority email intended for a single recipient (a login link for example). `BroadcastEmail` is for emails to be sent to many recipients (a product announcement or a terms of service change). `OtherMessageStream` is in case you have created additional message streams.
-}
type MessageStream
    = TransactionalEmail
    | BroadcastEmail
    | OtherMessageStream String


{-| Possible error codes we might get back when trying to send an email.
Some are just normal HTTP errors and others are specific to the Postmark API.
-}
type SendEmailError
    = UnknownError { statusCode : Int, body : String }
    | PostmarkError PostmarkSendResponse
    | NetworkError
    | Timeout
    | BadUrl String


type SendEmailsError
    = UnknownError_ { statusCode : Int, body : String }
    | PostmarkErrors (Nonempty PostmarkSendResponse)
    | NetworkError_
    | Timeout_
    | BadUrl_ String


sendEmailTask : ApiKey -> PostmarkSend -> Task SendEmailError ()
sendEmailTask (ApiKey token) d =
    Http.task
        { method = "POST"
        , headers = [ Http.header "X-Postmark-Server-Token" token ]
        , url = endpoint ++ "/email"
        , body = Http.jsonBody (encodeEmail d)
        , resolver = jsonResolver
        , timeout = Nothing
        }


encodeEmail : PostmarkSend -> E.Value
encodeEmail d =
    let
        ( htmlContent, inlineImages ) =
            case d.body of
                BodyHtml html ->
                    Internal.toString html

                BodyBoth html _ ->
                    Internal.toString html

                BodyText _ ->
                    ( "", [] )

        inlineAttachments : Dict String { content : String, mimeType : String }
        inlineAttachments =
            inlineImages
                |> List.map
                    (\( filename, { content, imageType } ) ->
                        ( filename
                        , { content = VendoredBase64.fromBytes content |> Maybe.withDefault ""
                          , mimeType = Internal.mimeType imageType
                          }
                        )
                    )
                |> Dict.fromList

        allAttachments : Dict String { content : String, mimeType : String }
        allAttachments =
            Dict.union inlineAttachments d.attachments

        attachmentsList : List ( String, { content : String, mimeType : String } )
        attachmentsList =
            Dict.toList allAttachments
    in
    E.object
        ([ ( "From", E.string <| emailToString d.from )
         , ( "To", E.string <| emailsToString d.to )
         , ( "Subject", E.string <| String.Nonempty.toString d.subject )
         , ( "MessageStream"
           , E.string
                (case d.messageStream of
                    TransactionalEmail ->
                        "outbound"

                    BroadcastEmail ->
                        "broadcast"

                    OtherMessageStream stream ->
                        stream
                )
           )
         ]
            ++ (case d.body of
                    BodyHtml _ ->
                        [ ( "HtmlBody", E.string htmlContent ) ]

                    BodyText text ->
                        [ ( "TextBody", E.string text ) ]

                    BodyBoth _ text ->
                        [ ( "HtmlBody", E.string htmlContent )
                        , ( "TextBody", E.string text )
                        ]
               )
            ++ (case attachmentsList of
                    [] ->
                        []

                    _ ->
                        [ ( "Attachments", E.list encodeAttachment attachmentsList ) ]
               )
        )


{-| Send multiple emails in a single API request. See more here <https://postmarkapp.com/developer/user-guide/send-email-with-api/batch-emails>
-}
sendEmailsTask : ApiKey -> Nonempty PostmarkSend -> Task SendEmailsError ()
sendEmailsTask (ApiKey token) d =
    Http.task
        { method = "POST"
        , headers = [ Http.header "X-Postmark-Server-Token" token ]
        , url = endpoint ++ "/email/batch"
        , body = List.Nonempty.toList d |> E.list encodeEmail |> Http.jsonBody
        , resolver = jsonResolver2
        , timeout = Nothing
        }


sendEmail : (Result SendEmailError () -> msg) -> ApiKey -> PostmarkSend -> Cmd msg
sendEmail msg token d =
    sendEmailTask token d |> Task.attempt msg


sendEmails : (Result SendEmailsError () -> msg) -> ApiKey -> Nonempty PostmarkSend -> Cmd msg
sendEmails msg token d =
    sendEmailsTask token d |> Task.attempt msg


{-| Attach files to the email. These will usually appear at the bottom of the email.

    import Bytes exposing (Bytes)
    import SendGrid

    attachPngImage : String -> Bytes -> Email -> Email
    attachPngImage name image email =
        SendGrid.addAttachments
            (Dict.fromList
                [ ( name ++ ".png"
                  , { content = image
                    , mimeType = "image/png"
                    }
                  )
                ]
            )
            email

If you want to include an image file within the body of your email, use `Email.Html.inlinePngImg`, `Email.Html.inlineJpegImg`, or `Email.Html.inlineGifImg` instead.

-}
addAttachments : Dict String { content : Bytes, mimeType : String } -> PostmarkSend -> PostmarkSend
addAttachments attachments email =
    { email
        | attachments =
            Dict.union
                (Dict.map
                    (\_ attachment ->
                        { content = VendoredBase64.fromBytes attachment.content |> Maybe.withDefault ""
                        , mimeType = attachment.mimeType
                        }
                    )
                    attachments
                )
                email.attachments
    }


emailsToString : List.Nonempty.Nonempty { name : String, email : EmailAddress } -> String
emailsToString nonEmptyEmails =
    nonEmptyEmails
        |> List.Nonempty.toList
        |> List.map emailToString
        |> String.join ", "


emailToString : { name : String, email : EmailAddress } -> String
emailToString address =
    if address.name == "" then
        EmailAddress2.toString address.email

    else
        String.filter (\char -> char /= '<' && char /= '>' && char /= ',') address.name
            ++ " <"
            ++ EmailAddress2.toString address.email
            ++ ">"


type alias PostmarkSendResponse =
    { errorCode : Int
    , message : String
    , to : List EmailAddress
    }


decodePostmarkSendResponse : D.Decoder PostmarkSendResponse
decodePostmarkSendResponse =
    D.map3 PostmarkSendResponse
        (D.field "ErrorCode" D.int)
        (D.field "Message" D.string)
        (optionalField "To" decodeEmails
            |> D.map
                (\a ->
                    case a of
                        Just nonempty ->
                            List.Nonempty.toList nonempty

                        Nothing ->
                            []
                )
        )


{-| Copied from <https://package.elm-lang.org/packages/elm-community/json-extra/latest/Json-Decode-Extra>
-}
optionalField : String -> D.Decoder a -> D.Decoder (Maybe a)
optionalField fieldName decoder =
    let
        finishDecoding json =
            case D.decodeValue (D.field fieldName D.value) json of
                Ok val ->
                    -- The field is present, so run the decoder on it.
                    D.map Just (D.field fieldName decoder)

                Err _ ->
                    -- The field was missing, which is fine!
                    D.succeed Nothing
    in
    D.value
        |> D.andThen finishDecoding


decodeEmails : D.Decoder (Nonempty EmailAddress)
decodeEmails =
    D.andThen
        (\text ->
            let
                emails : List ( String, Maybe EmailAddress )
                emails =
                    String.split "," text
                        |> List.filterMap
                            (\subtext ->
                                let
                                    trimmed =
                                        String.trim subtext
                                in
                                if trimmed == "" then
                                    Nothing

                                else
                                    Just ( trimmed, EmailAddress2.fromString trimmed )
                            )

                validEmails : List EmailAddress
                validEmails =
                    List.filterMap Tuple.second emails

                invalidEmails : List String
                invalidEmails =
                    List.filterMap
                        (\( subtext, maybeValid ) ->
                            if maybeValid == Nothing then
                                Just subtext

                            else
                                Nothing
                        )
                        emails
            in
            case invalidEmails of
                [] ->
                    case List.Nonempty.fromList validEmails of
                        Just nonempty ->
                            D.succeed nonempty

                        Nothing ->
                            D.fail "Expected at least one email"

                [ invalidEmail ] ->
                    invalidEmail ++ " is not a valid email" |> D.fail

                _ ->
                    invalidEmails
                        |> String.join ", "
                        |> (\a -> a ++ " are not valid emails")
                        |> D.fail
        )
        D.string



-- Helpers


jsonResolver : Http.Resolver SendEmailError ()
jsonResolver =
    let
        decodeBody metadata body =
            case D.decodeString decodePostmarkSendResponse body of
                Ok json ->
                    if json.errorCode == 0 then
                        Ok ()

                    else
                        PostmarkError json |> Err

                Err _ ->
                    UnknownError { statusCode = metadata.statusCode, body = body } |> Err
    in
    Http.stringResolver
        (\response ->
            case response of
                Http.GoodStatus_ metadata body ->
                    decodeBody metadata body

                Http.BadUrl_ message ->
                    Err (BadUrl message)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    decodeBody metadata body
        )


decodeNonempty : D.Decoder a -> D.Decoder (Nonempty a)
decodeNonempty decoder =
    D.list decoder
        |> D.andThen
            (\list ->
                case List.Nonempty.fromList list of
                    Just nonempty ->
                        D.succeed nonempty

                    Nothing ->
                        D.fail "Didn't get back any results"
            )


jsonResolver2 : Http.Resolver SendEmailsError ()
jsonResolver2 =
    let
        decoder : D.Decoder (Nonempty PostmarkSendResponse)
        decoder =
            D.oneOf
                [ decodeNonempty decodePostmarkSendResponse
                , decodePostmarkSendResponse |> D.map (\a -> Nonempty a [])
                ]

        decodeBody metadata body =
            case D.decodeString decoder body of
                Ok list ->
                    let
                        results : Nonempty (Result PostmarkSendResponse ())
                        results =
                            List.Nonempty.map
                                (\json ->
                                    if json.errorCode == 0 then
                                        Ok ()

                                    else
                                        Err json
                                )
                                list

                        badResults : List PostmarkSendResponse
                        badResults =
                            List.Nonempty.toList results
                                |> List.filterMap
                                    (\a ->
                                        case a of
                                            Ok _ ->
                                                Nothing

                                            Err error ->
                                                Just error
                                    )
                    in
                    case List.Nonempty.fromList badResults of
                        Just nonempty ->
                            PostmarkErrors nonempty |> Err

                        Nothing ->
                            Ok ()

                Err _ ->
                    UnknownError_ { statusCode = metadata.statusCode, body = body } |> Err
    in
    Http.stringResolver
        (\response ->
            case response of
                Http.GoodStatus_ metadata body ->
                    decodeBody metadata body

                Http.BadUrl_ message ->
                    Err (BadUrl_ message)

                Http.Timeout_ ->
                    Err Timeout_

                Http.NetworkError_ ->
                    Err NetworkError_

                Http.BadStatus_ metadata body ->
                    decodeBody metadata body
        )


encodeAttachment : ( String, { content : String, mimeType : String } ) -> E.Value
encodeAttachment ( filename, attachment ) =
    E.object
        [ ( "Name", E.string filename )
        , ( "Content", E.string attachment.content )
        , ( "ContentType", E.string attachment.mimeType )
        , ( "ContentID", E.string ("cid:" ++ filename) )
        ]
