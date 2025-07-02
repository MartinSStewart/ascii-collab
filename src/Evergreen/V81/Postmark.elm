module Evergreen.V81.Postmark exposing (..)

import Evergreen.V81.EmailAddress2


type alias PostmarkSendResponse =
    { errorCode : Int
    , message : String
    , to : List Evergreen.V81.EmailAddress2.EmailAddress
    }


type SendEmailError
    = UnknownError
        { statusCode : Int
        , body : String
        }
    | PostmarkError PostmarkSendResponse
    | NetworkError
    | Timeout
    | BadUrl String
