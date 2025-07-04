module Codegen.Parser exposing (ModuleInfo, ParsedFile, TypeDeclaration(..), parseFile)

import Codegen.ErrorWithLocation exposing (ErrorWithLocation)
import Codegen.TypeResult as TypeResult exposing (TypeResult(..))
import Dict exposing (Dict)
import Elm.Parser
import Elm.Processing
import Elm.RawFile as RawFile
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.File as File
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Fusion exposing (SpecialType(..), Type(..))
import Gen.CodeGen.Generate as Generate
import Result.Extra


type alias ParsedFile =
    Dict String TypeDeclaration


type alias ModuleInfo =
    { importAliases : Dict String ModuleName
    , importedTypes : Dict String ModuleName
    , moduleName : ModuleName
    , fullPath : String
    }


type TypeDeclaration
    = AliasDeclaration (List String) Type
    | CustomTypeDeclaration (List String) (List ( String, List Type ))
    | OpaqueTypeDeclaration (List String)


defaultImportAliases : Dict String ModuleName
defaultImportAliases =
    Dict.fromList [ ( "Cmd", [ "Platform", "Cmd" ] ), ( "Sub", [ "Platform", "Sub" ] ) ]


defaultTypeImports : Dict String ModuleName
defaultTypeImports =
    [ ( "Int", [ "Basics" ] )
    , ( "Float", [ "Basics" ] )
    , ( "Order", [ "Basics" ] )
    , ( "Bool", [ "Basics" ] )
    , ( "Never", [ "Basics" ] )
    , ( "List", [ "List" ] )
    , ( "Maybe", [ "Maybe" ] )
    , ( "Result", [ "Result" ] )
    , ( "String", [ "String" ] )
    , ( "Char", [ "Char" ] )
    , ( "Program", [ "Platform" ] )
    , ( "Cmd", [ "Platform", "Cmd" ] )
    , ( "Sub", [ "Platform", "Sub" ] )
    ]
        |> Dict.fromList


parseFile : { package : Bool, fullPath : String } -> String -> Result ErrorWithLocation ParsedFile
parseFile package source =
    case Elm.Parser.parse source of
        Ok rawFile ->
            process package rawFile

        Err _ ->
            Err
                { title = "Failed to parse file " ++ package.fullPath
                , description = "The module was not a valid Elm file. Try running `lamdera make` to get a better error message."
                , location = Nothing
                }


process : { package : Bool, fullPath : String } -> RawFile.RawFile -> Result ErrorWithLocation ParsedFile
process { fullPath } rawFile =
    let
        file : File.File
        file =
            Elm.Processing.process Elm.Processing.init rawFile

        moduleInfo : ModuleInfo
        moduleInfo =
            { moduleName = moduleName
            , fullPath = fullPath
            , importAliases = importAliases
            , importedTypes = importedTypes
            }

        transformDeclaration : Node Declaration -> Result ErrorWithLocation (Maybe ( String, TypeDeclaration ))
        transformDeclaration (Node _ declaration) =
            case declaration of
                Declaration.AliasDeclaration { name, generics, typeAnnotation } ->
                    let
                        params : List String
                        params =
                            List.map Node.value generics
                    in
                    typeAnnotation
                        |> typeAnnotationToType [ "alias " ++ Node.value name ] moduleInfo
                        |> TypeResult.map
                            (\tipe ->
                                ( Node.value name
                                , AliasDeclaration params tipe
                                )
                            )
                        |> TypeResult.toResult

                Declaration.CustomTypeDeclaration { name, generics, constructors } ->
                    let
                        typeName : String
                        typeName =
                            Node.value name

                        params : List String
                        params =
                            List.map Node.value generics
                    in
                    if isExposed typeName then
                        constructors
                            |> TypeResult.combineMap transformConstructor
                            |> TypeResult.map
                                (\variants ->
                                    ( Node.value name
                                    , CustomTypeDeclaration
                                        params
                                        variants
                                    )
                                )
                            |> TypeResult.toResult

                    else
                        Ok (Just ( typeName, OpaqueTypeDeclaration params ))

                _ ->
                    Ok Nothing

        isExposed : String -> Bool
        isExposed typeName =
            case exposingList of
                Exposing.Explicit exposes ->
                    exposes
                        |> List.any
                            (\(Node _ expose_) ->
                                case expose_ of
                                    Exposing.TypeExpose { name, open } ->
                                        name == typeName && open /= Nothing

                                    _ ->
                                        False
                            )

                Exposing.All _ ->
                    True

        transformConstructor : Node ValueConstructor -> TypeResult ( String, List Type )
        transformConstructor (Node _ { name, arguments }) =
            arguments
                |> TypeResult.combineMap (typeAnnotationToType [ "constructor " ++ Node.value name ] moduleInfo)
                |> TypeResult.map (\args -> ( Node.value name, args ))

        ( importAliases, importedTypes ) =
            file.imports
                |> List.foldl
                    (\(Node _ import_) ( aliasAcc, typesAcc ) ->
                        let
                            (Node _ importModuleName) =
                                import_.moduleName
                        in
                        ( case import_.moduleAlias of
                            Just (Node _ [ alias_ ]) ->
                                Dict.insert alias_ importModuleName aliasAcc

                            Nothing ->
                                aliasAcc

                            Just _ ->
                                -- This will get rejected by the compiler
                                aliasAcc
                        , case import_.exposingList of
                            Just (Node _ (Exposing.Explicit exposes)) ->
                                List.foldl
                                    (\(Node _ expose_) acc ->
                                        case expose_ of
                                            Exposing.TypeOrAliasExpose name ->
                                                Dict.insert name importModuleName acc

                                            Exposing.TypeExpose { name } ->
                                                Dict.insert name importModuleName acc

                                            _ ->
                                                acc
                                    )
                                    typesAcc
                                    exposes

                            _ ->
                                typesAcc
                        )
                    )
                    ( defaultImportAliases, defaultTypeImports )

        ( moduleName, exposingList ) =
            case file.moduleDefinition of
                Node _ (Module.NormalModule def) ->
                    ( Node.value def.moduleName, Node.value def.exposingList )

                Node _ (Module.PortModule def) ->
                    ( Node.value def.moduleName, Node.value def.exposingList )

                Node _ (Module.EffectModule def) ->
                    ( Node.value def.moduleName, Node.value def.exposingList )
    in
    file.declarations
        |> Result.Extra.combineMap transformDeclaration
        |> Result.map
            (\decls ->
                decls
                    |> List.filterMap identity
                    |> Dict.fromList
            )


typeAnnotationToType :
    List String
    -> ModuleInfo
    -> Node TypeAnnotation
    -> TypeResult Type
typeAnnotationToType context ({ importAliases, importedTypes, fullPath } as module_) (Node range type_) =
    let
        err : Generate.Error -> TypeResult x
        err e =
            { title = e.title
            , description = e.description
            , location =
                Just
                    { moduleName = module_.moduleName
                    , range = range
                    , fullPath = fullPath
                    }
            }
                |> TypeErr

        go : String -> Node TypeAnnotation -> TypeResult Type
        go fragment node =
            typeAnnotationToType (fragment :: context) module_ node
    in
    case type_ of
        TypeAnnotation.Record fields ->
            fields
                |> TypeResult.combineMap
                    (\(Node _ ( Node _ fieldName, fieldType )) ->
                        TypeResult.map
                            (\tipe ->
                                ( fieldName
                                , tipe
                                )
                            )
                            (go ("field " ++ fieldName) fieldType)
                    )
                |> TypeResult.map TRecord

        TypeAnnotation.GenericType name ->
            TypeOk (TVar name)

        TypeAnnotation.Typed (Node _ ( moduleName, typeName )) args ->
            args
                |> TypeResult.combineMap (go "arg")
                |> TypeResult.map
                    (\typeArgs ->
                        let
                            resolvedModuleName : ModuleName
                            resolvedModuleName =
                                case moduleName of
                                    [ single ] ->
                                        Dict.get single importAliases
                                            |> Maybe.withDefault moduleName

                                    [] ->
                                        Dict.get typeName importedTypes
                                            |> Maybe.withDefault module_.moduleName

                                    _ ->
                                        moduleName

                            specialType : Maybe SpecialType
                            specialType =
                                case ( resolvedModuleName, typeName, typeArgs ) of
                                    ( [ "Basics" ], "Int", [] ) ->
                                        Just TInt

                                    ( [ "Basics" ], "Float", [] ) ->
                                        Just TFloat

                                    ( [ "Basics" ], "Order", [] ) ->
                                        Just TOrder

                                    ( [ "Basics" ], "Bool", [] ) ->
                                        Just TBool

                                    ( [ "Basics" ], "Never", [] ) ->
                                        Just TNever

                                    ( [ "List" ], "List", [ child ] ) ->
                                        Just (TList child)

                                    ( [ "Maybe" ], "Maybe", [ child ] ) ->
                                        Just (TMaybe child)

                                    ( [ "Result" ], "Result", [ e, ok ] ) ->
                                        Just (TResult e ok)

                                    ( [ "String" ], "String", [] ) ->
                                        Just TString

                                    ( [ "Char" ], "Char", [] ) ->
                                        Just TChar

                                    ( [ "Set" ], "Set", [ child ] ) ->
                                        Just (TSet child)

                                    ( [ "Dict" ], "Dict", [ key, value ] ) ->
                                        Just (TDict key value)

                                    ( [ "Bytes" ], "Bytes", [] ) ->
                                        Just TBytes

                                    ( [ "Json", "Decode" ], "Value", [] ) ->
                                        Just TJson

                                    ( [ "Json", "Encode" ], "Value", [] ) ->
                                        Just TJson

                                    -- ( [ "SeqDict" ], "SeqDict", [ key, value ] ) ->
                                    --     Just (TSeqDict key value)
                                    _ ->
                                        Nothing
                        in
                        TNamed resolvedModuleName typeName typeArgs specialType
                    )

        TypeAnnotation.Unit ->
            TypeOk TUnit

        TypeAnnotation.Tupled [ l, r ] ->
            TypeResult.map2 TTuple
                (go "first" l)
                (go "second" r)

        TypeAnnotation.Tupled [ l, m, r ] ->
            TypeResult.map3 TTriple
                (go "first" l)
                (go "second" m)
                (go "third" r)

        TypeAnnotation.Tupled _ ->
            { title = "Invalid tuple type"
            , description = "Found a tuple type with an invalid number of arguments.\nThis should not happen for valid Elm code. Try compiling your code to get a better error message.\n\nIn context: " ++ String.join " -> " (List.reverse context)
            }
                |> err

        TypeAnnotation.GenericRecord (Node _ var) (Node _ fields) ->
            fields
                |> TypeResult.combineMap
                    (\(Node _ ( Node _ fieldName, fieldType )) ->
                        TypeResult.map
                            (\tipe ->
                                ( fieldName
                                , tipe
                                )
                            )
                            (go ("field " ++ fieldName) fieldType)
                    )
                |> TypeResult.map (TGenericRecord var)

        TypeAnnotation.FunctionTypeAnnotation _ _ ->
            TypeFunction
