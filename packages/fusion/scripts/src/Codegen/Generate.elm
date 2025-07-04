module Codegen.Generate exposing (CustomOrRecord(..), GenerateContent, customOrRecordToString, fqTypeNameToString, generate, mainTypeDict, typeDict)

import Bytes
import Bytes.Decode
import Codegen.CodegenResult as CodegenResult exposing (CodegenResult(..), Error)
import Codegen.Monad as Monad exposing (Context, Monad)
import Codegen.Parser
import Dict exposing (Dict)
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Let
import Elm.Op
import Elm.Syntax.ModuleName exposing (ModuleName)
import Fusion exposing (SpecialType(..), Type(..), Value(..))
import Fusion.Patch as Patch
import Gen.Basics
import Gen.Bytes.Encode
import Gen.Debug
import Gen.Dict
import Gen.Fusion
import Gen.Fusion.Patch
import Gen.List
import Gen.Result
import Gen.Result.Extra
import List.Extra
import Set exposing (Set)


type alias GenerateContent =
    { declarations : Dict ModuleName (Dict String Elm.Declaration)
    , typeQueue : List ( ModuleName, CustomOrRecord )
    , typeName : ( CustomOrRecord, List String )
    }


type CustomOrRecord
    = Custom String
    | Record (List ( String, Type ))


generate :
    { a | debug : Bool }
    -> Dict ModuleName Codegen.Parser.ParsedFile
    -> ( ModuleName, CustomOrRecord )
    -> CodegenResult GenerateContent
generate flags modules ( moduleName, typeName ) =
    case Dict.get moduleName modules of
        Nothing ->
            CodegenLoadFile moduleName

        Just module_ ->
            let
                context : Context
                context =
                    { debug = flags.debug
                    , modules = modules
                    , currentModule = moduleName
                    }
            in
            case typeName of
                Record fields ->
                    innerGenerate typeName (getParamsFromFields fields) (TRecord fields) context

                Custom custom ->
                    let
                        typeNameString : String
                        typeNameString =
                            customOrRecordToString typeName
                    in
                    case Dict.get typeNameString module_ of
                        Just (Codegen.Parser.AliasDeclaration params aliasType) ->
                            innerGenerate typeName params aliasType context

                        Just (Codegen.Parser.CustomTypeDeclaration params variants) ->
                            innerGenerate typeName params (TCustom custom params variants) context

                        Just (Codegen.Parser.OpaqueTypeDeclaration params) ->
                            { declarations = Dict.empty
                            , typeQueue = []
                            , typeName = ( typeName, params )
                            }
                                |> CodegenOk

                        Nothing ->
                            { title = "Missing type"
                            , body =
                                [ "Could not find type " ++ custom ++ " in module " ++ String.join "." moduleName ++ "."
                                , "It contained " ++ String.join ", " (Dict.keys module_)
                                ]
                                    |> String.join "\n"
                            }
                                |> CodegenErr


getParamsFromFields : List ( String, Type ) -> List String
getParamsFromFields fields =
    let
        getParams : Type -> List String
        getParams type_ =
            case type_ of
                TNamed _ _ children _ ->
                    List.concatMap getParams children

                TVar a ->
                    [ a ]

                TUnit ->
                    []

                TTuple l r ->
                    List.concatMap getParams [ l, r ]

                TTriple l m r ->
                    List.concatMap getParams [ l, m, r ]

                TCustom _ params children ->
                    params ++ List.concatMap (\( _, vars ) -> List.concatMap getParams vars) children

                TRecord childFields ->
                    getParamsFromFields childFields

                TGenericRecord _ childFields ->
                    getParamsFromFields childFields
    in
    List.concatMap (\( _, fieldType ) -> getParams fieldType) fields
        |> List.Extra.unique


customOrRecordToString : CustomOrRecord -> String
customOrRecordToString customOrRecord =
    case customOrRecord of
        Custom custom ->
            custom

        Record fields ->
            Tuple.first (recordToName fields)


innerGenerate :
    CustomOrRecord
    -> List String
    -> Type
    -> Monad GenerateContent
innerGenerate typeName params tipe context =
    let
        toValueExpression : Monad Elm.Expression
        toValueExpression =
            typeToValue tipe
                |> Monad.map
                    (\toValue ->
                        Elm.function
                            (patchersParams ++ [ ( "value", Just typeAnnotation ) ])
                            (\args ->
                                let
                                    value : Elm.Expression
                                    value =
                                        args
                                            |> List.reverse
                                            |> List.head
                                            |> Maybe.withDefault (Elm.val "value")
                                in
                                toValue value
                                    |> Elm.withType Gen.Fusion.annotation_.value
                            )
                    )

        patchExpression : Monad Elm.Expression
        patchExpression =
            typeToPatch tipe
                |> Monad.map
                    (\toPatch ->
                        Elm.function
                            (patchersParams
                                ++ [ ( "options", Just optionsType )
                                   , ( "patch", Just Gen.Fusion.Patch.annotation_.patch )
                                   , ( "value", Just typeAnnotation )
                                   ]
                            )
                            (\patchersAndPatch ->
                                case List.reverse patchersAndPatch of
                                    value :: patch :: options :: _ ->
                                        toPatch { options = options, patch = patch, value = value }
                                            |> Elm.withType (Elm.Annotation.result Gen.Fusion.Patch.annotation_.error typeAnnotation)

                                    _ ->
                                        -- This is unreachable
                                        Gen.Debug.todo "Something went wrong in generating patcher, list too short"
                            )
                    )

        buildExpression : Monad Elm.Expression
        buildExpression =
            typeToBuild tipe
                |> Monad.map
                    (\build ->
                        Elm.function
                            (List.map
                                (\paramName ->
                                    ( paramName ++ "Patcher"
                                    , Just <| Gen.Fusion.Patch.annotation_.patcher (Elm.Annotation.var paramName)
                                    )
                                )
                                params
                                ++ [ ( "value", Just Gen.Fusion.annotation_.value ) ]
                            )
                            (\patchersAndPatch ->
                                case List.reverse patchersAndPatch of
                                    patch :: _ ->
                                        build patch
                                            |> Elm.withType (Elm.Annotation.result Gen.Fusion.Patch.annotation_.error typeAnnotation)

                                    [] ->
                                        -- This is unreachable
                                        Gen.Debug.todo "Something went wrong in generating builder, empty list"
                            )
                    )

        typeAnnotation : Elm.Annotation.Annotation
        typeAnnotation =
            case typeName of
                Custom custom ->
                    Elm.Annotation.namedWith
                        context.currentModule
                        custom
                        (List.map Elm.Annotation.var params)

                Record fields ->
                    typeToAnnotation context.currentModule (TRecord fields)

        typeNameString : String
        typeNameString =
            customOrRecordToString typeName

        declareValue : String -> Elm.Expression -> ( String, Elm.Declaration )
        declareValue key value =
            ( key, Elm.expose <| Elm.declaration key value )

        maybeDeclareValue : String -> Monad Elm.Expression -> CodegenResult ( String, Elm.Declaration )
        maybeDeclareValue key value =
            value context
                |> CodegenResult.map (\v -> declareValue key v)
                |> CodegenResult.onError (\e -> errFromExpression e (declareValue key) context)

        patchersParams : List ( String, Maybe Elm.Annotation.Annotation )
        patchersParams =
            toPatchersParams params
    in
    [ maybeDeclareValue ("toValue_" ++ typeNameString) toValueExpression
    , maybeDeclareValue ("patcher_" ++ typeNameString) (Monad.ok <| patcherExpression typeNameString params (Just typeAnnotation))
    , maybeDeclareValue ("patch_" ++ typeNameString) patchExpression
    , maybeDeclareValue ("build_" ++ typeNameString) buildExpression
    ]
        |> CodegenResult.combine
        |> CodegenResult.map
            (\list ->
                let
                    typeExpression : Elm.Expression
                    typeExpression =
                        tipe
                            |> typeToExpression
                            |> Elm.withType Gen.Fusion.annotation_.type_

                    typeQueue : List ( ModuleName, CustomOrRecord )
                    typeQueue =
                        toQueue True context.currentModule tipe

                    declarations : List ( List String, List ( String, Elm.Declaration ) )
                    declarations =
                        [ ( "Fusion" :: "Generated" :: "TypeDict" :: context.currentModule
                          , [ declareValue ("type_" ++ typeNameString) typeExpression ]
                          )
                        , ( "Fusion" :: "Generated" :: context.currentModule
                          , list
                          )
                        ]
                in
                { declarations =
                    declarations
                        |> List.map (\( k, v ) -> ( k, Dict.fromList v ))
                        |> Dict.fromList
                , typeQueue = typeQueue
                , typeName = ( typeName, params )
                }
            )


typeToAnnotation : ModuleName -> Type -> Elm.Annotation.Annotation
typeToAnnotation moduleName tipe =
    let
        go : Type -> Elm.Annotation.Annotation
        go =
            typeToAnnotation moduleName
    in
    case tipe of
        TNamed _ _ _ (Just TInt) ->
            Elm.Annotation.int

        TNamed _ _ _ (Just TFloat) ->
            Elm.Annotation.float

        TNamed _ _ _ (Just TString) ->
            Elm.Annotation.string

        TNamed _ _ _ (Just TBool) ->
            Elm.Annotation.bool

        TNamed _ _ _ (Just TChar) ->
            Elm.Annotation.char

        TNamed _ _ _ (Just TNever) ->
            Gen.Basics.annotation_.never

        TNamed _ _ _ (Just (TList child)) ->
            Elm.Annotation.list (go child)

        TNamed _ _ _ (Just (TMaybe child)) ->
            Elm.Annotation.maybe (go child)

        TNamed _ _ _ (Just (TResult e o)) ->
            Elm.Annotation.result (go e) (go o)

        TNamed childModuleName typeName params _ ->
            Elm.Annotation.namedWith childModuleName typeName (List.map go params)

        TVar a ->
            Elm.Annotation.var a

        TUnit ->
            Elm.Annotation.unit

        TTuple l r ->
            Elm.Annotation.tuple (go l) (go r)

        TTriple l m r ->
            Elm.Annotation.triple (go l) (go m) (go r)

        TCustom name vars _ ->
            Elm.Annotation.namedWith moduleName name (List.map Elm.Annotation.var vars)

        TRecord fields ->
            fields
                |> List.map (\( fieldName, fieldType ) -> ( fieldName, go fieldType ))
                |> Elm.Annotation.record

        TGenericRecord name fields ->
            fields
                |> List.map (\( fieldName, fieldType ) -> ( fieldName, go fieldType ))
                |> Elm.Annotation.extensible name


toPatchersParams : List String -> List ( String, Maybe Elm.Annotation.Annotation )
toPatchersParams params =
    List.map
        (\paramName ->
            ( paramName ++ "Patcher"
            , Just <| Gen.Fusion.Patch.annotation_.patcher (Elm.Annotation.var paramName)
            )
        )
        params


patcherExpression :
    String
    -> List String
    -> Maybe Elm.Annotation.Annotation
    -> Elm.Expression
patcherExpression typeName params typeAnnotation =
    Elm.function
        (toPatchersParams params)
        (\patchers ->
            [ ( "patch"
              , Elm.apply (Elm.val <| "patch_" ++ typeName) patchers
              )
            , ( "build"
              , Elm.apply (Elm.val <| "build_" ++ typeName) patchers
              )
            , ( "toValue"
              , Elm.apply (Elm.val <| "toValue_" ++ typeName) patchers
              )
            ]
                |> Elm.record
                |> (case typeAnnotation of
                        Just tipe ->
                            Elm.withType (Gen.Fusion.Patch.annotation_.patcher tipe)

                        Nothing ->
                            identity
                   )
        )


optionsType : Elm.Annotation.Annotation
optionsType =
    Elm.Annotation.record [ ( "force", Elm.Annotation.bool ) ]


typeToPatcher : Type -> Monad Elm.Expression
typeToPatcher tipe =
    case tipe of
        TNamed _ _ _ (Just TInt) ->
            Monad.ok Gen.Fusion.Patch.patcher_Int

        TNamed _ _ _ (Just TFloat) ->
            Monad.ok Gen.Fusion.Patch.patcher_Float

        TNamed _ _ _ (Just TString) ->
            Monad.ok Gen.Fusion.Patch.patcher_String

        TNamed _ _ _ (Just TBool) ->
            Monad.ok Gen.Fusion.Patch.patcher_Bool

        TNamed _ _ _ (Just TChar) ->
            Monad.ok Gen.Fusion.Patch.patcher_Char

        TNamed _ _ _ (Just TNever) ->
            Monad.ok Gen.Fusion.Patch.patcher_Never

        TNamed _ _ _ (Just TBytes) ->
            Monad.ok Gen.Fusion.Patch.patcher_Bytes

        TNamed _ _ _ (Just TJson) ->
            Monad.ok Gen.Fusion.Patch.patcher_Json

        TNamed _ _ _ (Just (TList child)) ->
            Monad.map Gen.Fusion.Patch.patcher_List
                (typeToPatcher child)

        TNamed _ _ _ (Just (TSet child)) ->
            Monad.map Gen.Fusion.Patch.patcher_Set
                (typeToPatcher child)

        TNamed _ _ _ (Just (TDict key value)) ->
            Monad.map2 Gen.Fusion.Patch.patcher_Dict
                (typeToPatcher key)
                (typeToPatcher value)

        TNamed moduleName typeName params (Just TOrder) ->
            callPatcher moduleName typeName params

        TNamed moduleName typeName params (Just (TMaybe _)) ->
            callPatcher moduleName typeName params

        TNamed moduleName typeName params (Just (TResult _ _)) ->
            callPatcher moduleName typeName params

        -- TNamed _ _ _ (Just (TSeqDict _ _)) ->
        --     Debug.todo "branch 'TNamed _ _ _ (Just (TSeqDict _ _))' not implemented"
        TNamed moduleName typeName params Nothing ->
            callPatcher moduleName typeName params

        TVar name ->
            Monad.ok <| Elm.val <| name ++ "Patcher"

        TUnit ->
            Monad.ok Gen.Fusion.Patch.patcher_Unit

        TTuple l r ->
            Monad.map2 Gen.Fusion.Patch.patcher_Tuple
                (typeToPatcher l)
                (typeToPatcher r)

        TTriple l m r ->
            Monad.map3 Gen.Fusion.Patch.patcher_Triple
                (typeToPatcher l)
                (typeToPatcher m)
                (typeToPatcher r)

        TCustom _ _ _ ->
            errExpression (CodegenResult.errorFromString "[typeToPatcher] branch 'TCustom _ _ _' not implemented")

        TRecord fields ->
            let
                ( recordName, fieldTypes ) =
                    recordToName fields
            in
            fieldTypes
                |> Monad.combineMap typeToPatcher
                |> Monad.map
                    (\patchers ->
                        [ ( "patch"
                          , Elm.apply (Elm.val <| "patch_" ++ recordName) patchers
                          )
                        , ( "build"
                          , Elm.apply (Elm.val <| "build_" ++ recordName) patchers
                          )
                        , ( "toValue"
                          , Elm.apply (Elm.val <| "toValue_" ++ recordName) patchers
                          )
                        ]
                            |> Elm.record
                    )

        TGenericRecord _ _ ->
            errExpression (CodegenResult.errorFromString "[typeToPatcher] branch 'TGenericRecord _ _' not implemented")


recordToName : List ( String, Type ) -> ( String, List Type )
recordToName fields =
    fields
        |> List.unzip
        |> Tuple.mapFirst (String.join "__")


errExpression : Error -> Monad Elm.Expression
errExpression error =
    errFromExpression error identity


errFromExpression : Error -> (Elm.Expression -> b) -> Monad b
errFromExpression error f { debug } =
    if debug then
        CodegenOk <| f <| Gen.Debug.todo error.body

    else
        CodegenErr error


{-| Converts a type to its patch function.
-}
typeToPatch : Type -> Monad ({ options : Elm.Expression, patch : Elm.Expression, value : Elm.Expression } -> Elm.Expression)
typeToPatch tipe =
    case tipe of
        TNamed _ _ _ (Just TInt) ->
            Monad.ok (basicPatch Gen.Fusion.Patch.call_.patch_Int)

        TNamed _ _ _ (Just TFloat) ->
            Monad.ok (basicPatch Gen.Fusion.Patch.call_.patch_Float)

        TNamed _ _ _ (Just TString) ->
            Monad.ok (basicPatch Gen.Fusion.Patch.call_.patch_String)

        TNamed _ _ _ (Just TBool) ->
            Monad.ok (basicPatch Gen.Fusion.Patch.call_.patch_Bool)

        TNamed _ _ _ (Just TChar) ->
            Monad.ok (basicPatch Gen.Fusion.Patch.call_.patch_Char)

        TNamed _ _ _ (Just TNever) ->
            Monad.ok (basicPatch Gen.Fusion.Patch.call_.patch_Never)

        TNamed _ _ _ (Just TBytes) ->
            Monad.ok (basicPatch Gen.Fusion.Patch.call_.patch_Bytes)

        TNamed _ _ _ (Just TJson) ->
            Monad.ok (basicPatch Gen.Fusion.Patch.call_.patch_Json)

        TNamed _ _ _ (Just (TList child)) ->
            Monad.map (\childp { options, patch, value } -> Gen.Fusion.Patch.call_.patch_List childp options patch value)
                (typeToPatcher child)

        TNamed _ _ _ (Just (TSet child)) ->
            Monad.map (\childp { options, patch, value } -> Gen.Fusion.Patch.call_.patch_Set childp options patch value)
                (typeToPatcher child)

        TNamed _ _ _ (Just (TDict keyType valueType)) ->
            Monad.map2 (\keyp valuep { options, patch, value } -> Gen.Fusion.Patch.call_.patch_Dict keyp valuep options patch value)
                (typeToPatcher keyType)
                (typeToPatcher valueType)

        TNamed moduleName typeName params (Just TOrder) ->
            callPatch moduleName typeName params

        TNamed moduleName typeName params (Just (TMaybe _)) ->
            callPatch moduleName typeName params

        TNamed moduleName typeName params (Just (TResult _ _)) ->
            callPatch moduleName typeName params

        -- TNamed _ _ _ (Just (TSeqDict _ _)) ->
        --     Debug.todo "branch 'TNamed _ _ _ (Just (TSeqDict _ _))' not implemented"
        TNamed moduleName typeName params Nothing ->
            callPatch moduleName typeName params

        TVar name ->
            Monad.ok <| \{ options, patch, value } -> Elm.apply (Elm.get "patch" <| Elm.val <| name ++ "Patcher") [ options, patch, value ]

        TUnit ->
            Monad.ok (basicPatch Gen.Fusion.Patch.call_.patch_Unit)

        TTuple l r ->
            Monad.map2 (\lp rp { options, patch, value } -> Gen.Fusion.Patch.call_.patch_Tuple lp rp options patch value)
                (typeToPatcher l)
                (typeToPatcher r)

        TTriple l m r ->
            Monad.map3 (\lp mp rp { options, patch, value } -> Gen.Fusion.Patch.call_.patch_Triple lp mp rp options patch value)
                (typeToPatcher l)
                (typeToPatcher m)
                (typeToPatcher r)

        TCustom _ _ variants ->
            patchCustom variants

        TRecord fields ->
            fields
                |> Monad.combineMap
                    (\( _, fieldType ) -> typeToPatch fieldType)
                |> Monad.map
                    (\fieldPatches { options, patch, value } ->
                        Gen.Fusion.Patch.call_.patch_Record
                            (Elm.fn3
                                (Elm.Arg.varWith "fieldName" Elm.Annotation.string)
                                (Elm.Arg.varWith "fieldPatch" Gen.Fusion.Patch.annotation_.patch)
                                (Elm.Arg.var "acc")
                             <| \patchFieldName fieldPatch acc ->
                             Elm.Case.string patchFieldName
                                 { cases =
                                     List.map2
                                         (\( fieldName, _ ) fieldPatcher ->
                                             ( fieldName
                                             , Elm.apply Gen.Result.values_.map
                                                 [ Elm.fn (Elm.Arg.var (deduplicateForPatchingRecord fieldName)) <| \newValue ->
                                                 Elm.updateRecord
                                                     [ ( fieldName
                                                       , newValue
                                                       )
                                                     ]
                                                     acc
                                                 , fieldPatcher
                                                     { options = options
                                                     , patch = fieldPatch
                                                     , value = Elm.get fieldName acc
                                                     }
                                                 ]
                                             )
                                         )
                                         fields
                                         fieldPatches
                                 , otherwise =
                                     Gen.Result.make_.err <|
                                         Gen.Fusion.Patch.make_.unexpectedField patchFieldName
                                 }
                            )
                            patch
                            value
                    )

        TGenericRecord _ _ ->
            errFromExpression (CodegenResult.errorFromString "typeToPatch: TGenericRecord") (\e _ -> e)


basicPatch :
    (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> { options : Elm.Expression, patch : Elm.Expression, value : Elm.Expression }
    -> Elm.Expression
basicPatch function { options, patch, value } =
    function options patch value


deduplicateForPatchingRecord : String -> String
deduplicateForPatchingRecord name =
    if List.member name [ "options", "patch", "value" ] then
        name ++ "_"

    else
        name


patchModuleNameFor : ModuleName -> String -> List String
patchModuleNameFor moduleName typeName =
    if isSpecialCase moduleName typeName then
        [ "Fusion", "Patch" ]

    else
        "Fusion" :: "Generated" :: moduleName


patchCustom : List ( String, List Type ) -> Monad ({ options : Elm.Expression, patch : Elm.Expression, value : Elm.Expression } -> Elm.Expression)
patchCustom variants =
    let
        isNewtype : Bool
        isNewtype =
            case variants of
                [ _ ] ->
                    True

                _ ->
                    False

        case_ :
            Monad
                ({ options : Elm.Expression, patch : Elm.Expression, value : Elm.Expression }
                 -> (Elm.Expression -> Elm.Expression)
                 -> Elm.Expression
                )
        case_ =
            variants
                |> Monad.combineMap
                    (\( variantName, variantParams ) ->
                        variantParams
                            |> Monad.combineMap
                                (\variantParam ->
                                    Monad.map2
                                        (\build patcher ->
                                            { tipe = variantParam
                                            , build = build
                                            , patcher = patcher
                                            }
                                        )
                                        (typeToBuild variantParam)
                                        (typeToPatcher variantParam)
                                )
                            |> Monad.map (Tuple.pair variantName)
                    )
                |> Monad.andThen
                    (\variantsWithBuilds ->
                        variantsWithBuilds
                            |> Monad.combineMap (\variant context -> CodegenOk (variantToBranches isNewtype context variant))
                            |> Monad.map
                                (\combined { options, patch, value } isCorrect ->
                                    let
                                        wrongSame : Elm.Case.Branch
                                        wrongSame =
                                            Elm.Case.branch
                                                (Elm.Arg.triple
                                                    Elm.Arg.ignore
                                                    (Elm.Arg.customType "Fusion.Patch.PCustomSame"
                                                        (\_ _ -> ())
                                                        |> Elm.Arg.item Elm.Arg.ignore
                                                        |> Elm.Arg.item Elm.Arg.ignore
                                                    )
                                                    Elm.Arg.ignore
                                                )
                                                (\_ -> err (Patch.WrongType "patchCustom.wrongSame"))

                                        ( patchBranches, buildBranches ) =
                                            combined
                                                |> List.map (\f -> f isCorrect options)
                                                |> List.unzip
                                                |> Tuple.mapBoth List.concat List.concat
                                    in
                                    (patchBranches
                                        ++ wrongSame
                                        :: buildBranches
                                        ++ [ Elm.Case.branch Elm.Arg.ignore (\_ -> err (Patch.WrongType "patchCustom.lastBranch")) ]
                                    )
                                        |> Elm.Case.custom
                                            (Elm.triple value patch (Elm.get "force" options))
                                            (Elm.Annotation.triple
                                                Elm.Annotation.unit
                                                Gen.Fusion.Patch.annotation_.patch
                                                Elm.Annotation.bool
                                            )
                                )
                    )
    in
    if isNewtype then
        case_
            |> Monad.map
                (\case__ blob ->
                    case__ blob identity
                )

    else
        let
            isCorrectVariant : Context -> ({ b | value : Elm.Expression } -> Elm.Expression -> Elm.Expression)
            isCorrectVariant context { value } expectedName =
                let
                    patterns =
                        variants
                            |> List.map
                                (\( variantName, variantParams ) ->
                                    let
                                        pattern : Elm.Arg (List Elm.Expression)
                                        pattern =
                                            variantToPattern context.currentModule
                                                variantName
                                                (List.map
                                                    (\_ -> Elm.Arg.ignore)
                                                    variantParams
                                                )
                                    in
                                    Elm.Case.branch
                                        (Elm.Arg.tuple
                                            pattern
                                            (Elm.Arg.string variantName)
                                        )
                                        (\_ -> Elm.bool True)
                                )
                in
                (patterns
                    ++ [ Elm.Case.branch Elm.Arg.ignore (\_ -> Elm.bool False) ]
                )
                    |> Elm.Case.custom (Elm.tuple value expectedName)
                        (Elm.Annotation.tuple Elm.Annotation.unit Elm.Annotation.string)
        in
        Monad.map2 Tuple.pair
            case_
            (\context -> CodegenOk (isCorrectVariant context))
            |> Monad.map
                (\( case__, isCorrectVariant_ ) blob ->
                    case__ blob
                        |> Elm.Let.letIn
                        |> Elm.Let.fn "isCorrectVariant"
                            (Elm.Arg.varWith "expected" Elm.Annotation.string)
                            (isCorrectVariant_ blob)
                        |> Elm.Let.toExpression
                )


variantToBranches :
    Bool
    -> Context
    ->
        ( String
        , List
            { tipe : Type
            , build : Elm.Expression -> Elm.Expression
            , patcher : Elm.Expression
            }
        )
    -> (Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    ->
        ( List Elm.Case.Branch
        , List Elm.Case.Branch
        )
variantToBranches isNewtype context ( variantName, variantParams ) isCorrect options =
    let
        patchesArg : Bool -> Elm.Arg (List Elm.Expression)
        patchesArg requireJust =
            Elm.Arg.list identity
                |> Elm.Arg.items
                    (List.map
                        (\i ->
                            let
                                var : Elm.Arg Elm.Expression
                                var =
                                    Elm.Arg.var ("patch" ++ String.fromInt i)
                            in
                            if requireJust then
                                Elm.Arg.customType "Just" identity
                                    |> Elm.Arg.item var

                            else
                                var
                        )
                        (List.range 0 (List.length variantParams - 1))
                    )

        changePattern : Elm.Case.Branch
        changePattern =
            Elm.Case.branch
                (Elm.Arg.triple
                    Elm.Arg.ignore
                    (Elm.Arg.customType "Fusion.Patch.PCustomChange"
                        (\expectedVariant _ args -> ( expectedVariant, args ))
                        |> Elm.Arg.item (Elm.Arg.var "expectedVariant")
                        |> Elm.Arg.item (Elm.Arg.string variantName)
                        |> Elm.Arg.item
                            (Elm.Arg.list identity
                                |> Elm.Arg.items
                                    (List.map
                                        (\i ->
                                            Elm.Arg.var
                                                ("arg" ++ String.fromInt i)
                                        )
                                        (List.range 0 (List.length variantParams - 1))
                                    )
                            )
                    )
                    Elm.Arg.ignore
                )
                (\( _, ( expectedVariant, patches ), _ ) ->
                    Elm.ifThen
                        (Elm.Op.or (Elm.get "force" options) <| isCorrect expectedVariant)
                        (arbitraryResultMap
                            (Elm.value
                                { importFrom = context.currentModule
                                , name = variantName
                                , annotation = Nothing
                                }
                            )
                            (List.map2 (\{ build } patch -> build patch)
                                variantParams
                                patches
                            )
                            -- Work around a type inference time explosion
                            |> Elm.withType (Elm.Annotation.maybe (Elm.Annotation.var "a"))
                        )
                        (err Patch.Conflict)
                )

        samePattern : Elm.Case.Branch
        samePattern =
            let
                pattern : Elm.Arg (List Elm.Expression)
                pattern =
                    variantToPattern context.currentModule
                        variantName
                        (List.indexedMap
                            (\i _ -> Elm.Arg.var ("arg" ++ String.fromInt i))
                            variantParams
                        )
            in
            Elm.Case.branch
                (Elm.Arg.triple
                    pattern
                    (Elm.Arg.customType "Fusion.Patch.PCustomSame" (\_ args -> args)
                        |> Elm.Arg.item (Elm.Arg.string variantName)
                        |> Elm.Arg.item (patchesArg False)
                    )
                    Elm.Arg.ignore
                )
                (\( args, patches, _ ) ->
                    arbitraryResultMap
                        (Elm.value
                            { importFrom = context.currentModule
                            , name = variantName
                            , annotation = Nothing
                            }
                        )
                        (List.map3
                            (\{ patcher } ->
                                Gen.Fusion.Patch.call_.maybeApply patcher options
                            )
                            variantParams
                            patches
                            args
                        )
                        -- Work around a type inference time explosion
                        |> Elm.withType (Elm.Annotation.maybe (Elm.Annotation.var "a"))
                )

        samePatternConflict : Elm.Case.Branch
        samePatternConflict =
            Elm.Case.branch
                (Elm.Arg.triple
                    Elm.Arg.ignore
                    (Elm.Arg.customType "Fusion.Patch.PCustomSame"
                        (\_ args -> args)
                        |> Elm.Arg.item (Elm.Arg.string variantName)
                        |> Elm.Arg.item Elm.Arg.ignore
                    )
                    (Elm.Arg.customType "False" ())
                )
                (\_ -> err Patch.Conflict)

        samePatternBuild : Elm.Case.Branch
        samePatternBuild =
            Elm.Case.branch
                (Elm.Arg.triple
                    Elm.Arg.ignore
                    (Elm.Arg.customType "Fusion.Patch.PCustomSame"
                        (\_ args -> args)
                        |> Elm.Arg.item (Elm.Arg.string variantName)
                        |> Elm.Arg.item (patchesArg True)
                    )
                    Elm.Arg.ignore
                )
                (\( _, patches, _ ) ->
                    arbitraryResultMap
                        (Elm.value
                            { importFrom = context.currentModule
                            , name = variantName
                            , annotation = Nothing
                            }
                        )
                        (List.map2
                            (\{ build } patch ->
                                Gen.Fusion.Patch.buildFromPatch build patch
                            )
                            variantParams
                            patches
                        )
                        -- Work around a type inference time explosion
                        |> Elm.withType (Elm.Annotation.maybe (Elm.Annotation.var "a"))
                )

        samePatternUnbuildable : Elm.Case.Branch
        samePatternUnbuildable =
            Elm.Case.branch
                (Elm.Arg.triple
                    Elm.Arg.ignore
                    (Elm.Arg.customType "Fusion.Patch.PCustomSame" (\_ _ -> ())
                        |> Elm.Arg.item (Elm.Arg.string variantName)
                        |> Elm.Arg.item Elm.Arg.ignore
                    )
                    Elm.Arg.ignore
                )
                (\_ -> err Patch.CouldNotBuildValueFromPatch)
    in
    if isNewtype then
        ( if List.isEmpty variantParams then
            [ samePattern ]

          else
            [ samePattern, samePatternUnbuildable ]
        , []
        )

    else
        ( if List.isEmpty variantParams then
            [ samePattern, samePatternConflict, samePatternBuild ]

          else
            [ samePattern, samePatternConflict, samePatternBuild, samePatternUnbuildable ]
        , [ changePattern ]
        )


err : Patch.Error -> Elm.Expression
err error =
    let
        errorToExpression : Patch.Error -> Elm.Expression
        errorToExpression e =
            case e of
                Patch.Conflict ->
                    Gen.Fusion.Patch.make_.conflict

                Patch.WrongType location ->
                    Gen.Fusion.Patch.make_.wrongType (Elm.string location)

                Patch.MissingField field ->
                    Gen.Fusion.Patch.make_.missingField (Elm.string field)

                Patch.UnexpectedField field ->
                    Gen.Fusion.Patch.make_.unexpectedField (Elm.string field)

                Patch.MissingValue ->
                    Gen.Fusion.Patch.make_.missingValue

                Patch.CouldNotBuildValueFromPatch ->
                    Gen.Fusion.Patch.make_.couldNotBuildValueFromPatch

                Patch.ErrorAtField field child ->
                    Gen.Fusion.Patch.make_.errorAtField (Elm.string field) (errorToExpression child)

                Patch.ErrorAtFirst child ->
                    Gen.Fusion.Patch.make_.errorAtFirst (errorToExpression child)

                Patch.ErrorAtSecond child ->
                    Gen.Fusion.Patch.make_.errorAtSecond (errorToExpression child)

                Patch.ErrorAtThird child ->
                    Gen.Fusion.Patch.make_.errorAtThird (errorToExpression child)

                Patch.ErrorAtCustomArgument index child ->
                    Gen.Fusion.Patch.make_.errorAtCustomArgument
                        (Elm.int index)
                        (errorToExpression child)

                Patch.ErrorAtIndex index child ->
                    Gen.Fusion.Patch.make_.errorAtIndex
                        (Elm.int index)
                        (errorToExpression child)

                Patch.ErrorAtKey key child ->
                    Gen.Fusion.Patch.make_.errorAtKey
                        (valueToExpression key)
                        (errorToExpression child)

                Patch.ErrorAtValueWithKey key child ->
                    Gen.Fusion.Patch.make_.errorAtValueWithKey
                        (valueToExpression key)
                        (errorToExpression child)
    in
    Gen.Result.make_.err (errorToExpression error)


valueToExpression : Value -> Elm.Expression
valueToExpression value =
    case value of
        VInt i ->
            Gen.Fusion.make_.vInt (Elm.int i)

        VFloat f ->
            Gen.Fusion.make_.vFloat (Elm.float f)

        VString s ->
            Gen.Fusion.make_.vString (Elm.string s)

        VBool b ->
            Gen.Fusion.make_.vBool (Elm.bool b)

        VChar c ->
            Gen.Fusion.make_.vChar (Elm.char c)

        VUnit ->
            Gen.Fusion.make_.vUnit

        VBytes bytes ->
            let
                listDecoder : Bytes.Decode.Decoder (List Int)
                listDecoder =
                    Bytes.Decode.loop ( Bytes.width bytes, [] ) stepDecode

                stepDecode :
                    ( Int, List Int )
                    ->
                        Bytes.Decode.Decoder
                            (Bytes.Decode.Step
                                ( Int, List Int )
                                (List Int)
                            )
                stepDecode ( count, acc ) =
                    if count <= 0 then
                        List.reverse acc
                            |> Bytes.Decode.Done
                            |> Bytes.Decode.succeed

                    else
                        Bytes.Decode.unsignedInt8
                            |> Bytes.Decode.map
                                (\byte ->
                                    Bytes.Decode.Loop ( count - 1, byte :: acc )
                                )
            in
            bytes
                |> Bytes.Decode.decode listDecoder
                |> Maybe.withDefault []
                |> List.map Elm.int
                |> Gen.List.map Gen.Bytes.Encode.call_.unsignedInt8
                |> Gen.Bytes.Encode.call_.sequence
                |> Gen.Bytes.Encode.encode
                |> Gen.Fusion.make_.vBytes

        VTuple l r ->
            Gen.Fusion.make_.vTuple
                (valueToExpression l)
                (valueToExpression r)

        VTriple l m r ->
            Gen.Fusion.make_.vTriple
                (valueToExpression l)
                (valueToExpression m)
                (valueToExpression r)

        VList { cursor, items } ->
            Gen.Fusion.make_.vList
                (Elm.record
                    [ ( "cursor", Elm.int cursor )
                    , ( "items", Elm.list (List.map valueToExpression items) )
                    ]
                )

        VSet { cursor, items } ->
            Gen.Fusion.make_.vSet
                (Elm.record
                    [ ( "cursor", Elm.int cursor )
                    , ( "items", Elm.list (List.map valueToExpression items) )
                    ]
                )

        VDict { cursor, items } ->
            Gen.Fusion.make_.vSet
                (Elm.record
                    [ ( "cursor", Elm.int cursor )
                    , ( "items"
                      , Elm.list
                            (List.map
                                (\( key, item ) ->
                                    Elm.tuple
                                        (valueToExpression key)
                                        (valueToExpression item)
                                )
                                items
                            )
                      )
                    ]
                )

        VPartialString { length, partial } ->
            Gen.Fusion.make_.vList
                (Elm.record
                    [ ( "length", Elm.int length )
                    , ( "partial", Elm.string partial )
                    ]
                )

        VCustom name children ->
            children
                |> List.map valueToExpression
                |> Elm.list
                |> Gen.Fusion.make_.vCustom (Elm.string name)

        VRecord fields ->
            fields
                |> Dict.toList
                |> List.map
                    (\( key, item ) ->
                        Elm.tuple
                            (Elm.string key)
                            (valueToExpression item)
                    )
                |> Gen.Dict.fromList
                |> Gen.Fusion.make_.vRecord

        VUnloaded ->
            Gen.Fusion.make_.vUnloaded


variantToPattern : ModuleName -> String -> List (Elm.Arg Elm.Expression) -> Elm.Arg (List Elm.Expression)
variantToPattern moduleName variantName args =
    let
        fullVariantName : String
        fullVariantName =
            fqTypeNameToString ( moduleName, variantName )
    in
    Elm.Arg.customType fullVariantName identity
        |> Elm.Arg.items args


callToValue : ModuleName -> String -> List Type -> Monad (Elm.Expression -> Elm.Expression)
callToValue moduleName typeName params =
    call "toValue" moduleName typeName params
        |> Monad.map (\f value -> Elm.apply f [ value ])


callPatch : ModuleName -> String -> List Type -> Monad ({ options : Elm.Expression, patch : Elm.Expression, value : Elm.Expression } -> Elm.Expression)
callPatch moduleName typeName params =
    call "patch" moduleName typeName params
        |> Monad.map (\f { options, patch, value } -> Elm.apply f [ options, patch, value ])


callBuild : ModuleName -> String -> List Type -> Monad (Elm.Expression -> Elm.Expression)
callBuild moduleName typeName params =
    call "build" moduleName typeName params
        |> Monad.map (\f patch -> Elm.apply f [ patch ])


callPatcher : ModuleName -> String -> List Type -> Monad Elm.Expression
callPatcher moduleName typeName params =
    call "patcher" moduleName typeName params


call : String -> ModuleName -> String -> List Type -> Monad Elm.Expression
call name moduleName typeName params =
    params
        |> Monad.combineMap typeToPatcher
        |> Monad.andThen
            (\patchers ->
                let
                    value : Monad Elm.Expression
                    value context =
                        case Dict.get moduleName context.modules of
                            Nothing ->
                                CodegenLoadFile moduleName

                            Just module_ ->
                                let
                                    actualModuleName : List String
                                    actualModuleName =
                                        patchModuleNameFor moduleName typeName
                                in
                                case
                                    Dict.get typeName module_
                                of
                                    Just (Codegen.Parser.OpaqueTypeDeclaration _) ->
                                        Elm.value
                                            { annotation = Nothing
                                            , importFrom =
                                                case actualModuleName of
                                                    "Fusion" :: "Generated" :: rest ->
                                                        "Fusion" :: rest

                                                    _ ->
                                                        actualModuleName
                                            , name = name ++ "_" ++ typeName
                                            }
                                            |> CodegenOk

                                    Nothing ->
                                        CodegenErr
                                            { title = "Missing type"
                                            , body = "Type " ++ fqTypeNameToString ( moduleName, typeName ) ++ "is missing "
                                            }

                                    _ ->
                                        Elm.value
                                            { annotation = Nothing
                                            , importFrom = actualModuleName
                                            , name = name ++ "_" ++ typeName
                                            }
                                            |> CodegenOk
                in
                if List.isEmpty patchers then
                    value

                else
                    Monad.map (\v -> Elm.apply v patchers) value
            )


{-| Converts a type to its build function.
-}
typeToBuild : Type -> Monad (Elm.Expression -> Elm.Expression)
typeToBuild tipe =
    case tipe of
        TNamed _ _ _ (Just TInt) ->
            Monad.ok Gen.Fusion.Patch.build_Int

        TNamed _ _ _ (Just TFloat) ->
            Monad.ok Gen.Fusion.Patch.build_Float

        TNamed _ _ _ (Just TString) ->
            Monad.ok Gen.Fusion.Patch.build_String

        TNamed _ _ _ (Just TBool) ->
            Monad.ok Gen.Fusion.Patch.build_Bool

        TNamed _ _ _ (Just TChar) ->
            Monad.ok Gen.Fusion.Patch.build_Char

        TNamed _ _ _ (Just TNever) ->
            Monad.ok Gen.Fusion.Patch.build_Never

        TNamed _ _ _ (Just TBytes) ->
            Monad.ok Gen.Fusion.Patch.build_Bytes

        TNamed _ _ _ (Just TJson) ->
            Monad.ok Gen.Fusion.Patch.build_Json

        TNamed _ _ _ (Just (TList child)) ->
            Monad.map Gen.Fusion.Patch.build_List
                (typeToPatcher child)

        TNamed _ _ _ (Just (TSet child)) ->
            Monad.map Gen.Fusion.Patch.build_Set
                (typeToPatcher child)

        TNamed _ _ _ (Just (TDict keyPatcher valuePatcher)) ->
            Monad.map2 Gen.Fusion.Patch.build_Dict
                (typeToPatcher keyPatcher)
                (typeToPatcher valuePatcher)

        TNamed moduleName typeName params (Just TOrder) ->
            callBuild moduleName typeName params

        TNamed moduleName typeName params (Just (TMaybe _)) ->
            callBuild moduleName typeName params

        TNamed moduleName typeName params (Just (TResult _ _)) ->
            callBuild moduleName typeName params

        -- TNamed _ _ _ (Just (TSeqDict _ _)) ->
        --     Debug.todo "branch 'TNamed _ _ _ (Just (TSeqDict _ _))' not implemented"
        TNamed moduleName typeName params Nothing ->
            callBuild moduleName typeName params

        TVar name ->
            Monad.ok <| \value ->
            Elm.apply (Elm.get "build" <| Elm.val <| name ++ "Patcher") [ value ]

        TUnit ->
            Monad.ok Gen.Fusion.Patch.build_Unit

        TTuple l r ->
            Monad.map2 Gen.Fusion.Patch.build_Tuple
                (typeToPatcher l)
                (typeToPatcher r)

        TTriple l m r ->
            Monad.map3 Gen.Fusion.Patch.build_Triple
                (typeToPatcher l)
                (typeToPatcher m)
                (typeToPatcher r)

        TCustom _ _ variants ->
            buildCustom variants
                |> Monad.map
                    (\custom value ->
                        Elm.apply Gen.Fusion.Patch.values_.build_Custom
                            [ custom
                            , value
                            ]
                    )

        TRecord fields ->
            buildRecord fields
                |> Monad.map Gen.Fusion.Patch.build_Record

        TGenericRecord _ _ ->
            errFromExpression (CodegenResult.errorFromString "typeToBuild: TGenericRecord") (\e _ -> e)


buildRecord : List ( String, Type ) -> Monad (Elm.Expression -> Elm.Expression)
buildRecord fields =
    if List.isEmpty fields then
        (\_ ->
            Elm.record []
                |> Gen.Result.make_.ok
        )
            |> Monad.ok

    else
        let
            getter : String -> Type -> Monad (Elm.Expression -> Elm.Expression)
            getter fieldName fieldType =
                typeToBuild fieldType
                    |> Monad.map
                        (\fieldBuilder getFieldExpression ->
                            Gen.Result.andThen fieldBuilder (Elm.apply getFieldExpression [ Elm.string fieldName ])
                        )
        in
        fields
            |> Monad.combineMap (\( fieldName, fieldType ) -> getter fieldName fieldType)
            |> Monad.map
                (\getters getFieldExpression ->
                    let
                        recordBuilder : Elm.Expression
                        recordBuilder =
                            Elm.function
                                (fields
                                    |> List.map
                                        (\( fieldName, _ ) ->
                                            ( fieldName, Nothing )
                                        )
                                )
                                (\fieldValues ->
                                    Elm.record <|
                                        List.map2
                                            (\( fieldName, _ ) fieldValue ->
                                                ( fieldName, fieldValue )
                                            )
                                            fields
                                            fieldValues
                                )
                    in
                    arbitraryResultMap recordBuilder (List.map (\get -> get getFieldExpression) getters)
                )


buildCustom : List ( String, List Type ) -> Monad Elm.Expression
buildCustom variants =
    variants
        |> Monad.combineMap
            (\( _, variantParams ) ->
                Monad.combineMap typeToBuild variantParams
            )
        |> Monad.andThen
            (\builders context ->
                CodegenOk <|
                    Elm.fn2
                        (Elm.Arg.var "name")
                        (Elm.Arg.var "params")
                    <| \name params ->
                    let
                        normalBranches : List Elm.Case.Branch
                        normalBranches =
                            List.map2
                                (\( variantName, variantParams ) builder ->
                                    Elm.Case.branch
                                        (Elm.Arg.tuple
                                            (Elm.Arg.string variantName)
                                            (Elm.Arg.list identity
                                                |> Elm.Arg.items
                                                    (List.map
                                                        (\i ->
                                                            Elm.Arg.var
                                                                ("patch" ++ String.fromInt i)
                                                        )
                                                        (List.range 0 (List.length variantParams - 1))
                                                    )
                                            )
                                        )
                                        (\( _, res ) ->
                                            arbitraryResultMap
                                                (Elm.value
                                                    { importFrom = context.currentModule
                                                    , name = variantName
                                                    , annotation = Nothing
                                                    }
                                                )
                                                (List.map2 identity
                                                    builder
                                                    res
                                                )
                                                -- Work around a type inference time explosion
                                                |> Elm.withType
                                                    (Elm.Annotation.result
                                                        (Elm.Annotation.var "e")
                                                        (Elm.Annotation.var "a")
                                                    )
                                        )
                                )
                                variants
                                builders
                    in
                    normalBranches
                        ++ [ Elm.Case.branch Elm.Arg.ignore (\_ -> err (Patch.WrongType "buildCustom last branch")) ]
                        |> Elm.Case.custom (Elm.tuple name params)
                            (Elm.Annotation.tuple
                                Elm.Annotation.string
                                (Elm.Annotation.list Gen.Fusion.Patch.annotation_.patch)
                            )
            )


arbitraryResultMap : Elm.Expression -> List Elm.Expression -> Elm.Expression
arbitraryResultMap final steps =
    let
        fieldsCount : Int
        fieldsCount =
            List.length steps
    in
    if fieldsCount == 0 then
        Gen.Result.make_.ok final

    else if fieldsCount <= 5 then
        Elm.apply
            (Elm.value
                { importFrom = [ "Result" ]
                , name =
                    if fieldsCount == 1 then
                        "map"

                    else
                        "map" ++ String.fromInt fieldsCount
                , annotation = Nothing
                }
            )
            (final
                :: steps
            )

    else
        steps
            |> List.foldl
                (\g acc ->
                    Elm.Op.pipe
                        (Elm.functionReduced "acc" <| Gen.Result.Extra.andMap g)
                        acc
                )
                (Gen.Result.make_.ok final)


specialCases : Set ( ModuleName, String )
specialCases =
    [ ( [ "Array" ], "Array" )
    , ( [ "Basics" ], "Bool" )
    , ( [ "Basics" ], "Float" )
    , ( [ "Basics" ], "Int" )
    , ( [ "Char" ], "Char" )
    , ( [ "Dict" ], "Dict" )
    , ( [ "List" ], "List" )
    , ( [ "Set" ], "Set" )
    , ( [ "String" ], "String" )
    , ( [ "Time" ], "Posix" )
    ]
        |> Set.fromList


toQueue : Bool -> ModuleName -> Type -> List ( ModuleName, CustomOrRecord )
toQueue topLevel currentModule tipe =
    let
        go : Type -> List ( ModuleName, CustomOrRecord )
        go child =
            toQueue False currentModule child

        goList : List Type -> List ( ModuleName, CustomOrRecord )
        goList list =
            List.concatMap go list

        goRecord : List ( String, Type ) -> List ( ModuleName, CustomOrRecord )
        goRecord fields =
            let
                childTypes : List ( ModuleName, CustomOrRecord )
                childTypes =
                    fields
                        |> List.map Tuple.second
                        |> goList
            in
            if topLevel then
                childTypes

            else
                ( currentModule, Record fields ) :: childTypes
    in
    case tipe of
        TNamed _ _ args (Just (TList _)) ->
            goList args

        TNamed _ _ args (Just (TSet _)) ->
            goList args

        TNamed _ _ args (Just (TDict _ _)) ->
            goList args

        TNamed moduleName typeName args _ ->
            ( moduleName, Custom typeName ) :: goList args

        TVar _ ->
            []

        TUnit ->
            []

        TTuple l r ->
            goList [ l, r ]

        TTriple l m r ->
            goList [ l, m, r ]

        TCustom _ _ variants ->
            variants
                |> List.concatMap Tuple.second
                |> goList

        TRecord fields ->
            goRecord fields

        TGenericRecord _ fields ->
            goRecord fields


isSpecialCase : ModuleName -> String -> Bool
isSpecialCase moduleName typeName =
    Set.member ( moduleName, typeName ) specialCases


typeToExpression : Type -> Elm.Expression
typeToExpression tipe =
    case tipe of
        TNamed moduleName name args specialType ->
            Gen.Fusion.make_.tNamed
                (listWith Elm.string moduleName)
                (Elm.string name)
                (listWith typeToExpression args)
                (Elm.maybe (Maybe.map specialTypeToExpression specialType))

        TVar var ->
            Gen.Fusion.make_.tVar (Elm.string var)

        TUnit ->
            Gen.Fusion.make_.tUnit

        TTuple l r ->
            Gen.Fusion.make_.tTuple
                (typeToExpression l)
                (typeToExpression r)

        TTriple l m r ->
            Gen.Fusion.make_.tTriple
                (typeToExpression l)
                (typeToExpression m)
                (typeToExpression r)

        TCustom name params variants ->
            let
                variantToExpression : ( String, List Type ) -> Elm.Expression
                variantToExpression ( vname, vargs ) =
                    Elm.tuple
                        (Elm.string vname)
                        (listWith typeToExpression vargs)
            in
            Gen.Fusion.make_.tCustom
                (Elm.string name)
                (listWith Elm.string params)
                (listWith variantToExpression variants)

        TRecord fields ->
            fields
                |> listWith
                    (\( name, child ) ->
                        Elm.tuple
                            (Elm.string name)
                            (typeToExpression child)
                    )
                |> Gen.Fusion.make_.tRecord

        TGenericRecord var fields ->
            fields
                |> listWith
                    (\( name, child ) ->
                        Elm.tuple
                            (Elm.string name)
                            (typeToExpression child)
                    )
                |> Gen.Fusion.make_.tGenericRecord (Elm.string var)


specialTypeToExpression : SpecialType -> Elm.Expression
specialTypeToExpression type_ =
    case type_ of
        TInt ->
            Gen.Fusion.make_.tInt

        TFloat ->
            Gen.Fusion.make_.tFloat

        TString ->
            Gen.Fusion.make_.tString

        TBool ->
            Gen.Fusion.make_.tBool

        TChar ->
            Gen.Fusion.make_.tChar

        TNever ->
            Gen.Fusion.make_.tNever

        TBytes ->
            Gen.Fusion.make_.tBytes

        TJson ->
            Gen.Fusion.make_.tJson

        TOrder ->
            Gen.Fusion.make_.tOrder

        TList child ->
            Gen.Fusion.make_.tList (typeToExpression child)

        TMaybe child ->
            Gen.Fusion.make_.tMaybe (typeToExpression child)

        TResult e o ->
            Gen.Fusion.make_.tResult (typeToExpression e) (typeToExpression o)

        TSet child ->
            Gen.Fusion.make_.tSet (typeToExpression child)

        TDict key value ->
            Gen.Fusion.make_.tDict (typeToExpression key) (typeToExpression value)



-- TSeqDict key value ->
--     Gen.Fusion.make_.tSeqDict (typeToExpression key) (typeToExpression value)


typeToValue : Type -> Monad (Elm.Expression -> Elm.Expression)
typeToValue tipe =
    case tipe of
        TNamed _ _ _ (Just TInt) ->
            Monad.ok Gen.Fusion.make_.vInt

        TNamed _ _ _ (Just TFloat) ->
            Monad.ok Gen.Fusion.make_.vFloat

        TNamed _ _ _ (Just TString) ->
            Monad.ok Gen.Fusion.make_.vString

        TNamed _ _ _ (Just TBool) ->
            Monad.ok Gen.Fusion.make_.vBool

        TNamed _ _ _ (Just TChar) ->
            Monad.ok Gen.Fusion.make_.vChar

        TNamed _ _ _ (Just TNever) ->
            Monad.ok Gen.Basics.never

        TNamed _ _ _ (Just TBytes) ->
            Monad.ok Gen.Fusion.make_.vBytes

        TNamed _ _ _ (Just TJson) ->
            Monad.ok Gen.Fusion.make_.vString

        TNamed _ _ _ (Just (TList child)) ->
            Monad.map Gen.Fusion.Patch.call_.toValue_List
                (typeToPatcher child)

        TNamed _ _ _ (Just (TSet child)) ->
            Monad.map Gen.Fusion.Patch.call_.toValue_Set
                (typeToPatcher child)

        TNamed _ _ _ (Just (TDict keyType valueType)) ->
            Monad.map2 Gen.Fusion.Patch.toValue_Dict
                (typeToPatcher keyType)
                (typeToPatcher valueType)

        TNamed moduleName typeName params (Just TOrder) ->
            callToValue moduleName typeName params

        TNamed moduleName typeName params (Just (TMaybe _)) ->
            callToValue moduleName typeName params

        TNamed moduleName typeName params (Just (TResult _ _)) ->
            callToValue moduleName typeName params

        -- TNamed _ _ _ (Just (TSeqDict _ _)) ->
        --     Monad.ok <| \_ -> Gen.Debug.todo "branch 'TNamed _ _ _ (Just (TSeqDict _ _))' not implemented"
        TNamed moduleName typeName params Nothing ->
            callToValue moduleName typeName params

        TVar name ->
            Monad.ok <| \value -> Elm.apply (Elm.get "toValue" <| Elm.val <| name ++ "Patcher") [ value ]

        TUnit ->
            Monad.ok <| \_ -> Gen.Fusion.make_.vUnit

        TTuple l r ->
            Monad.map2 Gen.Fusion.Patch.call_.toValue_Tuple
                (typeToPatcher l)
                (typeToPatcher r)

        TTriple l m r ->
            Monad.map3 Gen.Fusion.Patch.call_.toValue_Triple
                (typeToPatcher l)
                (typeToPatcher m)
                (typeToPatcher r)

        TCustom _ _ variants ->
            let
                variantToBranch : ( String, List Type ) -> Monad Elm.Case.Branch
                variantToBranch ( variantName, variantParams ) =
                    Monad.map2
                        (\converters context ->
                            let
                                pattern =
                                    variantToPattern context.currentModule
                                        variantName
                                        (List.indexedMap
                                            (\i _ -> Elm.Arg.var ("arg" ++ String.fromInt i))
                                            variantParams
                                        )
                            in
                            Elm.Case.branch pattern
                                (\args ->
                                    Gen.Fusion.make_.vCustom
                                        (Elm.string variantName)
                                        (Elm.list <| List.map2 identity converters args)
                                )
                        )
                        (variantParams
                            |> Monad.combineMap typeToValue
                        )
                        CodegenOk
            in
            variants
                |> Monad.combineMap variantToBranch
                |> Monad.map (\branches value -> Elm.Case.custom value Elm.Annotation.unit branches)

        TRecord fields ->
            fields
                |> Monad.combineMap
                    (\( fieldName, fieldType ) ->
                        Monad.map
                            (\fieldToValue value ->
                                Elm.tuple
                                    (Elm.string fieldName)
                                    (fieldToValue (Elm.get fieldName value))
                            )
                            (typeToValue fieldType)
                    )
                |> Monad.map
                    (\list value ->
                        List.map (\f -> f value) list
                            |> Gen.Dict.fromList
                            |> Gen.Fusion.make_.vRecord
                    )

        TGenericRecord _ _ ->
            errFromExpression (CodegenResult.errorFromString "typeToValue: TGenericRecord") (\e _ -> e)


listWith : (a -> Elm.Expression) -> List a -> Elm.Expression
listWith f list =
    Elm.list (List.map f list)


fqTypeNameToString : ( ModuleName, String ) -> String
fqTypeNameToString ( moduleName, typeName ) =
    String.join "." moduleName ++ "." ++ typeName


typeDict : List ( CustomOrRecord, List String ) -> Elm.Declaration
typeDict declarations =
    declarations
        |> List.map
            (\( typeName, argNames ) ->
                let
                    name : String
                    name =
                        customOrRecordToString typeName
                in
                Elm.tuple
                    (Elm.string name)
                    (Elm.tuple
                        ((Elm.val <| "type_" ++ name)
                            |> Elm.withType Gen.Fusion.annotation_.type_
                        )
                        (argNames
                            |> List.map Elm.string
                            |> Elm.list
                        )
                    )
            )
        |> Gen.Dict.fromList
        |> Elm.declaration "typeDict"
        |> Elm.expose


mainTypeDict : Dict ModuleName ( Dict String Elm.Declaration, List ( CustomOrRecord, List String ) ) -> Elm.File
mainTypeDict declarations =
    declarations
        |> Dict.keys
        |> List.map (List.drop 2)
        |> List.filter (\moduleName -> List.head moduleName == Just "TypeDict")
        |> List.map
            (\moduleName ->
                Elm.tuple
                    (Elm.list <| List.map Elm.string moduleName)
                    (Elm.value
                        { importFrom = "Fusion" :: "Generated" :: moduleName
                        , name = "typeDict"
                        , annotation =
                            Just
                                (Elm.Annotation.dict
                                    Elm.Annotation.string
                                    (Elm.Annotation.tuple
                                        Gen.Fusion.annotation_.type_
                                        (Elm.Annotation.list Elm.Annotation.string)
                                    )
                                )
                        }
                    )
            )
        |> Gen.Dict.fromList
        |> Elm.declaration "typeDict"
        |> Elm.expose
        |> List.singleton
        |> Elm.file [ "Fusion", "Generated", "TypeDict" ]
