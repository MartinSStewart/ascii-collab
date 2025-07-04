module Codegen.CodegenResult exposing (CodegenResult(..), Error, andThen, andThen2, combine, combineMap, errorFromString, map, map2, map3, onError)

import Elm.Syntax.ModuleName exposing (ModuleName)


type CodegenResult a
    = CodegenOk a
    | CodegenErr Error
    | CodegenLoadFile ModuleName


type alias Error =
    { title : String
    , body : String
    }


errorFromString : String -> Error
errorFromString body =
    { title = "Custom error"
    , body = body
    }


map : (a -> b) -> CodegenResult a -> CodegenResult b
map f result =
    case result of
        CodegenOk v ->
            CodegenOk (f v)

        CodegenErr e ->
            CodegenErr e

        CodegenLoadFile moduleName ->
            CodegenLoadFile moduleName


map2 : (a -> b -> r) -> CodegenResult a -> CodegenResult b -> CodegenResult r
map2 f a b =
    case a of
        CodegenOk av ->
            map (f av) b

        CodegenErr e ->
            CodegenErr e

        CodegenLoadFile moduleName ->
            CodegenLoadFile moduleName


map3 : (a -> b -> c -> r) -> CodegenResult a -> CodegenResult b -> CodegenResult c -> CodegenResult r
map3 f a b c =
    case a of
        CodegenOk av ->
            map2 (f av) b c

        CodegenErr e ->
            CodegenErr e

        CodegenLoadFile moduleName ->
            CodegenLoadFile moduleName


andThen : (a -> CodegenResult b) -> CodegenResult a -> CodegenResult b
andThen f result =
    case result of
        CodegenOk v ->
            f v

        CodegenErr e ->
            CodegenErr e

        CodegenLoadFile moduleName ->
            CodegenLoadFile moduleName


andThen2 : (a -> b -> CodegenResult r) -> CodegenResult a -> CodegenResult b -> CodegenResult r
andThen2 f a b =
    case a of
        CodegenOk v ->
            andThen (f v) b

        CodegenErr e ->
            CodegenErr e

        CodegenLoadFile moduleName ->
            CodegenLoadFile moduleName


onError : (Error -> CodegenResult a) -> CodegenResult a -> CodegenResult a
onError f result =
    case result of
        CodegenOk v ->
            CodegenOk v

        CodegenErr e ->
            f e

        CodegenLoadFile moduleName ->
            CodegenLoadFile moduleName


combine : List (CodegenResult a) -> CodegenResult (List a)
combine list =
    combineHelp list []


combineHelp : List (CodegenResult a) -> List a -> CodegenResult (List a)
combineHelp list acc =
    case list of
        head :: tail ->
            case head of
                CodegenOk a ->
                    combineHelp tail (a :: acc)

                CodegenErr x ->
                    CodegenErr x

                CodegenLoadFile moduleName ->
                    CodegenLoadFile moduleName

        [] ->
            CodegenOk (List.reverse acc)


combineMap : (a -> CodegenResult b) -> List a -> CodegenResult (List b)
combineMap f ls =
    combineMapHelp f ls []


combineMapHelp : (a -> CodegenResult b) -> List a -> List b -> CodegenResult (List b)
combineMapHelp f list acc =
    case list of
        head :: tail ->
            case f head of
                CodegenOk a ->
                    combineMapHelp f tail (a :: acc)

                CodegenErr x ->
                    CodegenErr x

                CodegenLoadFile moduleName ->
                    CodegenLoadFile moduleName

        [] ->
            CodegenOk (List.reverse acc)
