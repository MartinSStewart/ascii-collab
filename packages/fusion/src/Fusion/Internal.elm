module Fusion.Internal exposing (boolToString, escapeString)

import Json.Encode


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


escapeString : String -> String
escapeString input =
    Json.Encode.encode 0 (Json.Encode.string input)
