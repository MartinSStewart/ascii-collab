module Evergreen.V53.Email exposing (..)


type alias Email =
    { localPart : String
    , tags : List String
    , domain : String
    , tld : List String
    }
