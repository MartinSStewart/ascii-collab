module TestNever exposing (suite)

import GenerationUtils
import Test exposing (Test)


suite : Test
suite =
    GenerationUtils.testGenerationForType "Result Never String"
        """
        type Model = Err Never | Ok String
        """
        [ """
        build_Model : Fusion.Value -> Result Fusion.Patch.Error FakeModule.Model
        build_Model value =
            Fusion.Patch.build_Custom
                (\\name params ->
                     case ( name, params ) of
                         ( "Err", [ patch0 ] ) ->
                             Result.map FakeModule.Err (Fusion.Patch.build_Never patch0)
        
                         ( "Ok", [ patch0 ] ) ->
                             Result.map FakeModule.Ok (Fusion.Patch.build_String patch0)
        
                         _ ->
                             Result.Err
                                 (Fusion.Patch.WrongType "buildCustom last branch")
                )
                value
        """, """
        patch_Model :
            { force : Bool }
            -> Fusion.Patch.Patch
            -> FakeModule.Model
            -> Result Fusion.Patch.Error FakeModule.Model
        patch_Model options patch value =
            let
                isCorrectVariant expected =
                    case ( value, expected ) of
                        ( FakeModule.Err _, "Err" ) ->
                            True

                        ( FakeModule.Ok _, "Ok" ) ->
                            True

                        _ ->
                            False
            in
            case ( value, patch, options.force ) of
                ( FakeModule.Err arg0, Fusion.Patch.PCustomSame "Err" [ patch0 ], _ ) ->
                    Result.map
                        FakeModule.Err
                        (Fusion.Patch.maybeApply
                            Fusion.Patch.patcher_Never
                            options
                            patch0
                            arg0
                        )

                ( _, Fusion.Patch.PCustomSame "Err" _, False ) ->
                    Result.Err Fusion.Patch.Conflict

                ( _, Fusion.Patch.PCustomSame "Err" [ (Just patch0) ], _ ) ->
                    Result.map
                        FakeModule.Err
                        (Fusion.Patch.buildFromPatch Fusion.Patch.build_Never patch0)

                ( _, Fusion.Patch.PCustomSame "Err" _, _ ) ->
                    Result.Err Fusion.Patch.CouldNotBuildValueFromPatch

                ( FakeModule.Ok arg0, Fusion.Patch.PCustomSame "Ok" [ patch0 ], _ ) ->
                    Result.map
                        FakeModule.Ok
                        (Fusion.Patch.maybeApply
                            Fusion.Patch.patcher_String
                            options
                            patch0
                            arg0
                        )

                ( _, Fusion.Patch.PCustomSame "Ok" _, False ) ->
                    Result.Err Fusion.Patch.Conflict

                ( _, Fusion.Patch.PCustomSame "Ok" [ (Just patch0) ], _ ) ->
                    Result.map
                        FakeModule.Ok
                        (Fusion.Patch.buildFromPatch Fusion.Patch.build_String patch0)

                ( _, Fusion.Patch.PCustomSame "Ok" _, _ ) ->
                    Result.Err Fusion.Patch.CouldNotBuildValueFromPatch

                ( _, Fusion.Patch.PCustomSame _ _, _ ) ->
                    Result.Err (Fusion.Patch.WrongType "patchCustom.wrongSame")

                ( _, Fusion.Patch.PCustomChange expectedVariant "Err" [ arg0 ], _ ) ->
                    if options.force || isCorrectVariant expectedVariant then
                        Result.map FakeModule.Err (Fusion.Patch.build_Never arg0)

                    else
                        Result.Err Fusion.Patch.Conflict

                ( _, Fusion.Patch.PCustomChange expectedVariant "Ok" [ arg0 ], _ ) ->
                    if options.force || isCorrectVariant expectedVariant then
                        Result.map FakeModule.Ok (Fusion.Patch.build_String arg0)

                    else
                        Result.Err Fusion.Patch.Conflict

                _ ->
                    Result.Err (Fusion.Patch.WrongType "patchCustom.lastBranch")
        """, """
        patcher_Model : Fusion.Patch.Patcher FakeModule.Model
        patcher_Model =
            { patch = patch_Model, build = build_Model, toValue = toValue_Model }
        """, """
        toValue_Model : FakeModule.Model -> Fusion.Value
        toValue_Model value =
            case value of
                FakeModule.Err arg0 ->
                    Fusion.VCustom "Err" [ Basics.never arg0 ]

                FakeModule.Ok arg0 ->
                    Fusion.VCustom "Ok" [ Fusion.VString arg0 ]
        """, """
        type_Model : Fusion.Type
        type_Model =
            Fusion.TCustom
                "Model"
                []
                [ ( "Err"
                  , [ Fusion.TNamed [ "Basics" ] "Never" [] (Just Fusion.TNever) ]
                  )
                , ( "Ok"
                  , [ Fusion.TNamed [ "String" ] "String" [] (Just Fusion.TString) ]
                  )
                ]
        """ ]
