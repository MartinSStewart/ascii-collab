module GenerationUtils exposing (testGenerationForFile, testGenerationForType)

import Ansi.Color
import Codegen.CodegenResult as CodegenResult
import Codegen.Generate as Generate
import Codegen.Parser
import Dict exposing (Dict)
import Diff
import Diff.ToString
import Elm
import Elm.ToString
import Expect
import String.Multiline
import Test exposing (Test, test)


testGenerationForType : String -> String -> List String -> Test
testGenerationForType label input outputs =
    test label <| \_ ->
    case
        Codegen.Parser.parseFile { package = False, fullPath = "<test>" }
            ("module " ++ fakeModule ++ " exposing (..)\n" ++ String.Multiline.here input)
    of
        Err e ->
            Expect.fail e.description

        Ok parsedFile ->
            testGenerationForFile parsedFile outputs


testGenerationForFile : Dict String Codegen.Parser.TypeDeclaration -> List String -> Expect.Expectation
testGenerationForFile parsedFile outputs =
    case Dict.toList parsedFile of
        [ ( name, _ ) ] ->
            case Generate.generate { debug = True, generateStubs = True } (Dict.singleton [ fakeModule ] parsedFile) ( [ fakeModule ], Generate.Custom name ) of
                CodegenResult.CodegenErr e ->
                    Expect.fail e.body

                CodegenResult.CodegenLoadFile missing ->
                    Expect.fail <| "Unexpected missing file: " ++ String.join "." missing

                CodegenResult.CodegenOk content ->
                    let
                        actual : List String
                        actual =
                            content.declarations
                                |> Dict.values
                                |> List.concatMap Dict.values
                                |> List.map declarationToString

                        expected : List String
                        expected =
                            List.map String.Multiline.here outputs

                        check : List String -> List String -> Expect.Expectation
                        check actualQueue expectedQueue =
                            case ( actualQueue, expectedQueue ) of
                                ( [], [] ) ->
                                    Expect.pass

                                ( actualHead :: actualTail, expectedHead :: expectedTail ) ->
                                    if equalsModuloWs actualHead expectedHead then
                                        check actualTail expectedTail

                                    else
                                        actualHead
                                            |> expectEqualMultiline expectedHead

                                ( [], _ ) ->
                                    Expect.fail "Less items than expected"

                                ( _, [] ) ->
                                    Expect.fail "More items than expected"
                    in
                    check actual expected

        _ ->
            Expect.fail "Use a single type as input"


expectEqualMultiline : String -> String -> Expect.Expectation
expectEqualMultiline exp actual =
    if exp == actual then
        Expect.pass

    else
        let
            header : String
            header =
                Ansi.Color.fontColor Ansi.Color.blue "Diff from expected to actual:"
        in
        Expect.fail
            (header
                ++ "\n"
                ++ (Diff.diffLinesWith
                        (Diff.defaultOptions
                            |> Diff.ignoreLeadingWhitespace
                        )
                        exp
                        actual
                        |> Diff.ToString.diffToString { context = 4, color = True }
                   )
            )


equalsModuloWs : String -> String -> Bool
equalsModuloWs actual expected =
    List.map String.trim (String.split "\n" actual)
        == List.map String.trim (String.split "\n" expected)


declarationToString : Elm.Declaration -> String
declarationToString declaration =
    Elm.ToString.declaration declaration
        |> List.map .body
        |> String.join "\n\n"


fakeModule : String
fakeModule =
    "FakeModule"
