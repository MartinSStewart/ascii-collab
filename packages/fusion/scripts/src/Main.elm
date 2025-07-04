module Main exposing (run)

{-| Harness for generating the editors.
-}

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Env
import BackendTask.File as File
import BackendTask.Glob as Glob
import BackendTask.Time
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Codegen.CodegenResult as CodegenResult
import Codegen.ErrorWithLocation as ErrorWithLocation
import Codegen.Generate as Generate exposing (CustomOrRecord(..))
import Codegen.Parser
import Console
import Dict exposing (Dict)
import Duration exposing (Duration)
import Elm
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Version
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Pages.Script.Spinner as Spinner
import Set exposing (Set)


type alias Model =
    { declarations :
        Dict
            ModuleName
            ( Dict String Elm.Declaration
            , List ( CustomOrRecord, List String )
            )
    , typeQueue : List ( ModuleName, CustomOrRecord )
    , files : Dict ModuleName Codegen.Parser.ParsedFile
    , flags : Flags
    , missing : Set ModuleName
    , modules : Dict ModuleName ModuleInfo
    , elmHome : String
    }


type alias ModuleInfo =
    { fullPath : String
    , package : Bool
    }


type alias Flags =
    { rootTypes : List ( ModuleName, CustomOrRecord )
    , debug : Bool
    , elmJson : String
    , outDir : String
    }


run : Script
run =
    Script.withCliOptions config mainLoop


config : Program.Config Flags
config =
    Program.config
        |> Program.add
            (OptionsParser.build
                (\elmJson outDir debug rootTypes ->
                    { rootTypes =
                        let
                            fromOptions : List ( ModuleName, CustomOrRecord )
                            fromOptions =
                                rootTypes
                                    |> List.filterMap
                                        (\rootType ->
                                            case List.reverse (String.split "." rootType) of
                                                [] ->
                                                    Nothing

                                                tail :: init ->
                                                    Just ( List.reverse init, Custom tail )
                                        )
                        in
                        if List.isEmpty fromOptions then
                            [ ( [ "Types" ], Custom "BackendModel" ) ]

                        else
                            fromOptions
                    , debug = debug
                    , elmJson = elmJson
                    , outDir = outDir
                    }
                )
                |> OptionsParser.with (Option.requiredPositionalArg "elm.json")
                |> OptionsParser.with (Option.requiredPositionalArg "outDir")
                |> OptionsParser.with (Option.flag "debug")
                |> OptionsParser.withRestArgs (Option.restArgs "types")
            )


mainLoop : Flags -> BackendTask FatalError ()
mainLoop flags =
    Do.do (logInfo "Starting fusion codegen") <| \_ ->
    Do.do getElmHomePath <| \elmHome ->
    Spinner.steps
        |> Spinner.withStep "Parsing main elm.json" (\_ -> parseMainElmJson flags)
        |> Spinner.withStep "Getting module list"
            (\elmJson ->
                Do.do (getAllModules flags elmJson elmHome) <| \modulesList ->
                Do.do (foldModules modulesList) <| \modules ->
                Do.do (logDebug { flags = flags } (modulesListDebugString modules)) <| \_ ->
                BackendTask.succeed modules
            )
        |> Spinner.withStep "Generating files" (generateFiles flags elmHome)
        |> Spinner.withStep "Writing files" (writeFiles flags)
        |> Spinner.runSteps


writeFiles : Flags -> ( List Elm.File, Set (List String) ) -> BackendTask FatalError ()
writeFiles flags ( files, missing ) =
    BackendTask.doEach
        [ logDebug { flags = flags } <| "Writing " ++ String.fromInt (List.length files) ++ " files"
        , files
            |> List.map (writeFile { flags = flags })
            |> BackendTask.combine
            |> BackendTask.map (\_ -> ())
        , if Set.isEmpty missing then
            Do.noop

          else
            logWarning <| missingModulesMessage missing
        ]


modulesListDebugString : Dict (List String) { package : Bool, fullPath : String } -> String
modulesListDebugString modules =
    modules
        |> Dict.toList
        |> List.map
            (\( moduleName, { fullPath } ) ->
                "  " ++ String.join "." moduleName ++ ": " ++ fullPath
            )
        |> (::) "Modules: "
        |> String.join "\n"


missingModulesMessage : Set (List String) -> String
missingModulesMessage missing =
    let
        missingCount : Int
        missingCount =
            Set.size missing

        missingCountString : String
        missingCountString =
            String.fromInt missingCount

        names : String
        names =
            Set.toList missing
                |> List.take 20
                |> List.map (String.join ".")
                |> String.join ", "

        tail : String
        tail =
            if missingCount < 20 then
                "."

            else
                "..."
    in
    "There were " ++ missingCountString ++ " missing modules: " ++ names ++ tail


parseMainElmJson : Flags -> BackendTask FatalError Elm.Project.ApplicationInfo
parseMainElmJson flags =
    flags.elmJson
        |> File.jsonFile Elm.Project.decoder
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\elmJson ->
                case elmJson of
                    Elm.Project.Application application ->
                        BackendTask.succeed application

                    Elm.Project.Package _ ->
                        BackendTask.fail <| FatalError.fromString "Packages are not supported yet"
            )


getElmHomePath : BackendTask FatalError String
getElmHomePath =
    BackendTask.Env.get "ELM_HOME"
        |> BackendTask.toResult
        |> BackendTask.andThen
            (\elmHome ->
                case elmHome of
                    Ok (Just home) ->
                        BackendTask.succeed home

                    _ ->
                        BackendTask.Env.expect "HOME"
                            |> BackendTask.allowFatal
                            |> BackendTask.map (\home -> home ++ "/.elm")
            )


getAllModules :
    Flags
    -> Elm.Project.ApplicationInfo
    -> String
    -> BackendTask FatalError (List (Dict ModuleName ModuleInfo))
getAllModules flags elmJson elmHome =
    elmJson.depsDirect
        |> List.map
            (\( authorAndName, version ) ->
                String.join "/"
                    [ elmHome
                    , "0.19.1"
                    , "packages"
                    , Elm.Package.toString authorAndName
                    , Elm.Version.toString version
                    , "elm.json"
                    ]
            )
        |> (::) flags.elmJson
        |> List.map (getModulesFromElmJson flags)
        |> BackendTask.combine


getModulesFromElmJson :
    Flags
    -> String
    -> BackendTask FatalError (Dict ModuleName ModuleInfo)
getModulesFromElmJson flags elmJsonPath =
    let
        model_ : { flags : Flags }
        model_ =
            { flags = flags }

        baseDir : String
        baseDir =
            case String.split "/" elmJsonPath of
                [ _ ] ->
                    "./"

                multiple ->
                    multiple
                        |> List.reverse
                        |> List.drop 1
                        |> (::) ""
                        |> List.reverse
                        |> String.join "/"
    in
    Do.do (logDebug model_ <| "Parsing " ++ elmJsonPath) <| \_ ->
    Do.do (File.jsonFile Elm.Project.decoder elmJsonPath |> BackendTask.allowFatal) <| \elmJson ->
    case elmJson of
        Elm.Project.Application application ->
            Do.do (logDebug model_ "It's an application's elm.json") <| \_ ->
            application.dirs
                |> List.map
                    (\dir ->
                        let
                            glob : Glob.Glob ( List String, { fullPath : String, package : Bool } )
                            glob =
                                Glob.succeed
                                    (\partialPath lastPiece fullPath ->
                                        ( partialPath ++ [ lastPiece ]
                                        , { fullPath = fullPath, package = False }
                                        )
                                    )
                                    |> Glob.match (Glob.literal (baseDir ++ dir ++ "/"))
                                    |> Glob.capture Glob.recursiveWildcard
                                    |> Glob.match (Glob.literal "/")
                                    |> Glob.capture Glob.wildcard
                                    |> Glob.match (Glob.literal ".elm")
                                    |> Glob.captureFilePath
                        in
                        Do.do (logDebug model_ <| "Globbing " ++ dir) <| \_ ->
                        glob |> Glob.toBackendTask
                    )
                |> BackendTask.combine
                |> BackendTask.map
                    (\files ->
                        files
                            |> List.concat
                            |> List.filter
                                (\( moduleName, _ ) ->
                                    case moduleName of
                                        "Fusion" :: "Generated" :: _ ->
                                            False

                                        "Evergreen" :: _ ->
                                            False

                                        _ ->
                                            True
                                )
                            |> Dict.fromList
                    )

        Elm.Project.Package package ->
            let
                fromList :
                    List Elm.Module.Name
                    -> BackendTask error (Dict ModuleName ModuleInfo)
                fromList list =
                    list
                        |> List.map
                            (\moduleName ->
                                ( String.split "." <| Elm.Module.toString moduleName
                                , { fullPath = baseDir ++ "src/" ++ String.replace "." "/" (Elm.Module.toString moduleName) ++ ".elm", package = True }
                                )
                            )
                        |> Dict.fromList
                        |> BackendTask.succeed
            in
            case package.exposed of
                Elm.Project.ExposedList moduleNames ->
                    fromList moduleNames

                Elm.Project.ExposedDict dict ->
                    dict
                        |> List.concatMap Tuple.second
                        |> fromList


foldModules : List (Dict ModuleName ModuleInfo) -> BackendTask FatalError (Dict ModuleName ModuleInfo)
foldModules modules =
    let
        mergeModules :
            Dict ModuleName ModuleInfo
            -> Dict ModuleName ModuleInfo
            -> Result FatalError (Dict ModuleName ModuleInfo)
        mergeModules e acc =
            foldlResult foldStep acc (Dict.toList e)

        foldStep :
            ( ModuleName, ModuleInfo )
            -> Dict ModuleName ModuleInfo
            -> Result FatalError (Dict ModuleName ModuleInfo)
        foldStep ( k, v1 ) iacc =
            case Dict.get k iacc of
                Nothing ->
                    Ok (Dict.insert k v1 iacc)

                Just v2 ->
                    Err <|
                        FatalError.fromString <|
                            "Module "
                                ++ String.join "." k
                                ++ " found in both "
                                ++ v1.fullPath
                                ++ " and "
                                ++ v2.fullPath
    in
    foldlResult mergeModules Dict.empty modules
        |> BackendTask.fromResult


foldlResult : (a -> b -> Result error b) -> b -> List a -> Result error b
foldlResult foldStep init list =
    case list of
        [] ->
            Ok init

        head :: tail ->
            case foldStep head init of
                Err e ->
                    Err e

                Ok next ->
                    foldlResult foldStep next tail


generateFiles : Flags -> String -> Dict ModuleName ModuleInfo -> BackendTask FatalError ( List Elm.File, Set ModuleName )
generateFiles flags elmHome modules =
    let
        go : Model -> BackendTask FatalError ( List Elm.File, Set ModuleName )
        go model =
            case step model of
                Done files ->
                    BackendTask.succeed ( files, model.missing )

                Running task ->
                    BackendTask.andThen go task
    in
    go
        { declarations = Dict.empty
        , typeQueue = flags.rootTypes
        , files = Dict.empty
        , flags = flags
        , missing = Set.empty
        , modules = modules
        , elmHome = elmHome
        }


writeFile : { a | flags : Flags } -> Elm.File -> BackendTask FatalError ()
writeFile model file =
    let
        fullPath : String
        fullPath =
            model.flags.outDir ++ "/" ++ file.path
    in
    Do.do (logDebug model ("Writing " ++ fullPath)) <| \_ ->
    Script.writeFile
        { path = fullPath
        , body = file.contents
        }
        |> BackendTask.allowFatal


upsert : comparable -> v -> (v -> v) -> Dict comparable v -> Dict comparable v
upsert key empty updater dict =
    Dict.insert key (updater <| Maybe.withDefault empty <| Dict.get key dict) dict


type Step
    = Running (BackendTask FatalError Model)
    | Done (List Elm.File)


step : Model -> Step
step model =
    case model.typeQueue of
        [] ->
            model.declarations
                |> Dict.toList
                |> List.map
                    (\( moduleName, ( declarations, types ) ) ->
                        declarations
                            |> Dict.values
                            |> (if List.take 3 moduleName == [ "Fusion", "Generated", "TypeDict" ] then
                                    (::) (Generate.typeDict types)

                                else
                                    identity
                               )
                            |> Elm.file moduleName
                    )
                |> (::) (Generate.mainTypeDict model.declarations)
                |> Done

        (( moduleName, typeName ) as head) :: tail ->
            (let
                typeNameString : String
                typeNameString =
                    Generate.customOrRecordToString typeName
             in
             case moduleName of
                [] ->
                    { title = "Empty module name"
                    , body = "Error with type " ++ typeNameString
                    }
                        |> FatalError.build
                        |> BackendTask.fail

                _ ->
                    case
                        model.declarations
                            |> Dict.get ("Fusion" :: "Generated" :: "TypeDict" :: moduleName)
                            |> Maybe.andThen (Tuple.first >> Dict.get ("type_" ++ typeNameString))
                    of
                        Just _ ->
                            -- Do.do (logInfo model <| "Skipping " ++ Generate.fqTypeNameToString head ++ ", already generated") <| \_ ->
                            BackendTask.succeed { model | typeQueue = tail }

                        Nothing ->
                            case Generate.generate model.flags model.files head of
                                CodegenResult.CodegenOk result ->
                                    Do.do (logDebug model ("Generated " ++ Generate.fqTypeNameToString ( moduleName, typeNameString ))) <| \_ ->
                                    BackendTask.succeed
                                        { model
                                            | declarations =
                                                result.declarations
                                                    |> Dict.toList
                                                    |> List.foldl
                                                        (\( modName, declarations ) ->
                                                            upsert
                                                                modName
                                                                ( Dict.empty, [] )
                                                                (\( oldDecls, oldTypes ) ->
                                                                    ( Dict.union declarations oldDecls
                                                                    , result.typeName :: oldTypes
                                                                    )
                                                                )
                                                        )
                                                        model.declarations
                                            , typeQueue = result.typeQueue ++ tail
                                        }

                                CodegenResult.CodegenErr e ->
                                    BackendTask.fail <| FatalError.build e

                                CodegenResult.CodegenLoadFile missingModuleName ->
                                    Do.do (readFile model missingModuleName |> BackendTask.toResult) <| \result ->
                                    case result of
                                        Ok { package, fullPath, content } ->
                                            parseModule { package = package, fullPath = fullPath } missingModuleName content model

                                        Err { recoverable, fatal } ->
                                            case recoverable of
                                                File.FileDoesntExist ->
                                                    { model
                                                        | typeQueue = tail
                                                        , missing = Set.insert missingModuleName model.missing
                                                    }
                                                        |> BackendTask.succeed

                                                File.DecodingError ever ->
                                                    never ever

                                                File.FileReadError _ ->
                                                    BackendTask.fail fatal
            )
                |> Running


parseModule : { package : Bool, fullPath : String } -> ModuleName -> String -> Model -> BackendTask FatalError Model
parseModule package moduleName moduleSource model =
    Do.do (logDebug model <| "Parsing " ++ package.fullPath) <| \_ ->
    Do.do (timed_ <| \_ -> Codegen.Parser.parseFile package moduleSource) <| \( result, duration ) ->
    case result of
        Ok module_ ->
            let
                ms : Float
                ms =
                    Duration.inMilliseconds duration

                relativePath : String
                relativePath =
                    package.fullPath
                        |> relativeTo (dirname model.flags.elmJson)
                        |> relativeTo (model.elmHome ++ "/0.19.1")

                msg : String
                msg =
                    "Parsed " ++ relativePath ++ " in " ++ String.fromInt (round ms) ++ "ms"
            in
            (if ms > 500 then
                logError msg

             else if ms > 100 then
                logWarning msg

             else
                logDebug model msg
            )
                |> BackendTask.map
                    (\_ ->
                        { model | files = Dict.insert moduleName module_ model.files }
                    )

        Err e ->
            ErrorWithLocation.toError e
                |> BackendTask.fail


relativeTo : String -> String -> String
relativeTo root path =
    if String.startsWith root path then
        String.dropLeft (String.length root + 1) path

    else
        path


dirname : String -> String
dirname path =
    path
        |> String.split "/"
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        |> String.join "/"


timed_ : (() -> value) -> BackendTask error ( value, Duration )
timed_ task =
    Do.do BackendTask.Time.now <| \begin ->
    let
        result : value
        result =
            task ()
    in
    Do.do BackendTask.Time.now <| \end ->
    BackendTask.succeed ( result, Duration.from begin end )


readFile :
    Model
    -> ModuleName
    ->
        BackendTask
            { fatal : FatalError
            , recoverable : File.FileReadError error
            }
            { content : String, fullPath : String, package : Bool }
readFile model moduleName =
    case Dict.get moduleName model.modules of
        Just { fullPath, package } ->
            File.rawFile fullPath
                |> BackendTask.map
                    (\content ->
                        { content = content
                        , fullPath = fullPath
                        , package = package
                        }
                    )

        Nothing ->
            BackendTask.fail
                { fatal = FatalError.fromString <| "Module " ++ String.join "." moduleName ++ " not found"
                , recoverable = File.FileDoesntExist
                }


logInfo : String -> BackendTask error ()
logInfo message =
    Script.log <| Console.cyan message


logError : String -> BackendTask error ()
logError message =
    Script.log <| Console.red message


logWarning : String -> BackendTask error ()
logWarning message =
    Script.log <| Console.yellow message


logDebug : { a | flags : Flags } -> String -> BackendTask error ()
logDebug model message =
    if model.flags.debug then
        Script.log <| Console.dark <| Console.white message

    else
        Do.noop
