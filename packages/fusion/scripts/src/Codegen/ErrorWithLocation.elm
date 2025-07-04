module Codegen.ErrorWithLocation exposing (ErrorWithLocation, toError)

import Elm.Syntax.Range exposing (Range)
import FatalError exposing (FatalError)


type alias ErrorWithLocation =
    { title : String
    , description : String
    , location : Maybe { moduleName : List String, fullPath : String, range : Range }
    }


toError : ErrorWithLocation -> FatalError
toError error =
    { title = error.title
    , body =
        case error.location of
            Nothing ->
                error.description

            Just { moduleName, fullPath, range } ->
                error.description
                    ++ "\n\nWhile processing module "
                    ++ String.join "." moduleName
                    ++ ", at line "
                    ++ String.fromInt range.start.row
                    ++ ".\nIn file "
                    ++ fullPath
                    ++ "."
    }
        |> FatalError.build
