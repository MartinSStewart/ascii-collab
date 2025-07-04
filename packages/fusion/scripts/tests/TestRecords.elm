module TestRecords exposing (nuAshWorld, point)

import GenerationUtils
import Test exposing (Test)


nuAshWorld : Test
nuAshWorld =
    GenerationUtils.testGenerationForType "Record inside custom type #2"
        """
        type TickPerIntervalCurve
            = QuarterAndRest
                { quarter : Int
                , rest : Int
                }
            | Linear Int
        """
        [ """build_TickPerIntervalCurve :
    Fusion.Value -> Result Fusion.Patch.Error FakeModule.TickPerIntervalCurve
build_TickPerIntervalCurve value =
    Fusion.Patch.build_Custom
        (\\name params ->
             case ( name, params ) of
                 ( "QuarterAndRest", [ patch0 ] ) ->
                     Result.map
                         FakeModule.QuarterAndRest
                         (Fusion.Patch.build_Record
                              (\\build_RecordUnpack ->
                                   Result.map2
                                       (\\quarter rest ->
                                            { quarter = quarter, rest = rest }
                                       )
                                       (Result.andThen
                                            Fusion.Patch.build_Int
                                            (build_RecordUnpack "quarter")
                                       )
                                       (Result.andThen
                                            Fusion.Patch.build_Int
                                            (build_RecordUnpack "rest")
                                       )
                              )
                              patch0
                         )

                 ( "Linear", [ patch0 ] ) ->
                     Result.map
                         FakeModule.Linear
                         (Fusion.Patch.build_Int patch0)

                 _ ->
                     Result.Err
                         (Fusion.Patch.WrongType "buildCustom last branch")
        )
        value"""
        , """patch_TickPerIntervalCurve :
    { force : Bool }
    -> Fusion.Patch.Patch
    -> FakeModule.TickPerIntervalCurve
    -> Result Fusion.Patch.Error FakeModule.TickPerIntervalCurve
patch_TickPerIntervalCurve options patch value =
    let
        isCorrectVariant expected =
            case ( value, expected ) of
                ( FakeModule.QuarterAndRest _, "QuarterAndRest" ) ->
                    True

                ( FakeModule.Linear _, "Linear" ) ->
                    True

                _ ->
                    False
    in
    case ( value, patch, options.force ) of
        ( FakeModule.QuarterAndRest arg0, Fusion.Patch.PCustomSame "QuarterAndRest" [ patch0 ], _ ) ->
            Result.map
                FakeModule.QuarterAndRest
                (Fusion.Patch.maybeApply
                     { patch =
                         patch_quarter__rest
                             Fusion.Patch.patcher_Int
                             Fusion.Patch.patcher_Int
                     , build =
                         build_quarter__rest
                             Fusion.Patch.patcher_Int
                             Fusion.Patch.patcher_Int
                     , toValue =
                         toValue_quarter__rest
                             Fusion.Patch.patcher_Int
                             Fusion.Patch.patcher_Int
                     }
                     options
                     patch0
                     arg0
                )

        ( _, Fusion.Patch.PCustomSame "QuarterAndRest" _, False ) ->
            Result.Err Fusion.Patch.Conflict

        ( _, Fusion.Patch.PCustomSame "QuarterAndRest" [ (Just patch0) ], _ ) ->
            Result.map
                FakeModule.QuarterAndRest
                (Fusion.Patch.buildFromPatch
                     (Fusion.Patch.build_Record
                          (\\build_RecordUnpack ->
                               Result.map2
                                   (\\quarter rest ->
                                        { quarter = quarter, rest = rest }
                                   )
                                   (Result.andThen
                                        Fusion.Patch.build_Int
                                        (build_RecordUnpack "quarter")
                                   )
                                   (Result.andThen
                                        Fusion.Patch.build_Int
                                        (build_RecordUnpack "rest")
                                   )
                          )
                     )
                     patch0
                )

        ( _, Fusion.Patch.PCustomSame "QuarterAndRest" _, _ ) ->
            Result.Err Fusion.Patch.CouldNotBuildValueFromPatch

        ( FakeModule.Linear arg0, Fusion.Patch.PCustomSame "Linear" [ patch0 ], _ ) ->
            Result.map
                FakeModule.Linear
                (Fusion.Patch.maybeApply
                     Fusion.Patch.patcher_Int
                     options
                     patch0
                     arg0
                )

        ( _, Fusion.Patch.PCustomSame "Linear" _, False ) ->
            Result.Err Fusion.Patch.Conflict

        ( _, Fusion.Patch.PCustomSame "Linear" [ (Just patch0) ], _ ) ->
            Result.map
                FakeModule.Linear
                (Fusion.Patch.buildFromPatch Fusion.Patch.build_Int patch0)

        ( _, Fusion.Patch.PCustomSame "Linear" _, _ ) ->
            Result.Err Fusion.Patch.CouldNotBuildValueFromPatch

        ( _, Fusion.Patch.PCustomSame _ _, _ ) ->
            Result.Err (Fusion.Patch.WrongType "patchCustom.wrongSame")

        ( _, Fusion.Patch.PCustomChange expectedVariant "QuarterAndRest" [ arg0 ], _ ) ->
            if options.force || isCorrectVariant expectedVariant then
                Result.map
                    FakeModule.QuarterAndRest
                    (Fusion.Patch.build_Record
                         (\\build_RecordUnpack ->
                              Result.map2
                                  (\\quarter rest ->
                                       { quarter = quarter, rest = rest }
                                  )
                                  (Result.andThen
                                       Fusion.Patch.build_Int
                                       (build_RecordUnpack "quarter")
                                  )
                                  (Result.andThen
                                       Fusion.Patch.build_Int
                                       (build_RecordUnpack "rest")
                                  )
                         )
                         arg0
                    )

            else
                Result.Err Fusion.Patch.Conflict

        ( _, Fusion.Patch.PCustomChange expectedVariant "Linear" [ arg0 ], _ ) ->
            if options.force || isCorrectVariant expectedVariant then
                Result.map FakeModule.Linear (Fusion.Patch.build_Int arg0)

            else
                Result.Err Fusion.Patch.Conflict

        _ ->
            Result.Err (Fusion.Patch.WrongType "patchCustom.lastBranch")"""
        , """patcher_TickPerIntervalCurve :
    Fusion.Patch.Patcher FakeModule.TickPerIntervalCurve
patcher_TickPerIntervalCurve =
    { patch = patch_TickPerIntervalCurve
    , build = build_TickPerIntervalCurve
    , toValue = toValue_TickPerIntervalCurve
    }"""
        , """toValue_TickPerIntervalCurve : FakeModule.TickPerIntervalCurve -> Fusion.Value
toValue_TickPerIntervalCurve value =
    case value of
        FakeModule.QuarterAndRest arg0 ->
            Fusion.VCustom
                "QuarterAndRest"
                [ Fusion.VRecord
                    (Dict.fromList
                       [ ( "quarter", Fusion.VInt arg0.quarter )
                       , ( "rest", Fusion.VInt arg0.rest )
                       ]
                    )
                ]

        FakeModule.Linear arg0 ->
            Fusion.VCustom "Linear" [ Fusion.VInt arg0 ]"""
        , """type_TickPerIntervalCurve : Fusion.Type
type_TickPerIntervalCurve =
    Fusion.TCustom
        "TickPerIntervalCurve"
        []
        [ ( "QuarterAndRest"
          , [ Fusion.TRecord
                [ ( "quarter"
                  , Fusion.TNamed [ "Basics" ] "Int" [] (Just Fusion.TInt)
                  )
                , ( "rest"
                  , Fusion.TNamed [ "Basics" ] "Int" [] (Just Fusion.TInt)
                  )
                ]
            ]
          )
        , ( "Linear"
          , [ Fusion.TNamed [ "Basics" ] "Int" [] (Just Fusion.TInt) ]
          )
        ]"""
        ]


point : Test
point =
    GenerationUtils.testGenerationForType "Record inside custom type"
        """
        type Point3d units coordinates
            = Point3d { x : Float, y : Float, z : Float }
        """
        [ """
        build_Point3d :
            Fusion.Patch.Patcher units
            -> Fusion.Patch.Patcher coordinates
            -> Fusion.Value
            -> Result Fusion.Patch.Error (FakeModule.Point3d units coordinates)
        build_Point3d unitsPatcher coordinatesPatcher value =
            Fusion.Patch.build_Custom
                (\\name params ->
                    case ( name, params ) of
                        ( "Point3d", [ patch0 ] ) ->
                            Result.map
                                FakeModule.Point3d
                                (Fusion.Patch.build_Record
                                    (\\build_RecordUnpack ->
                                        Result.map3
                                            (\\x y z -> { x = x, y = y, z = z })
                                            (Result.andThen
                                                Fusion.Patch.build_Float
                                                (build_RecordUnpack "x")
                                            )
                                            (Result.andThen
                                                Fusion.Patch.build_Float
                                                (build_RecordUnpack "y")
                                            )
                                            (Result.andThen
                                                Fusion.Patch.build_Float
                                                (build_RecordUnpack "z")
                                            )
                                    )
                                    patch0
                                )

                        _ ->
                            Result.Err
                                (Fusion.Patch.WrongType "buildCustom last branch")
                )
                value
          """
        , """
        patch_Point3d :
            Fusion.Patch.Patcher units
            -> Fusion.Patch.Patcher coordinates
            -> { force : Bool }
            -> Fusion.Patch.Patch
            -> FakeModule.Point3d units coordinates
            -> Result Fusion.Patch.Error (FakeModule.Point3d units coordinates)
        patch_Point3d unitsPatcher coordinatesPatcher options patch value =
            case ( value, patch, options.force ) of
                ( FakeModule.Point3d arg0, Fusion.Patch.PCustomSame "Point3d" [ patch0 ], _ ) ->
                    Result.map
                        FakeModule.Point3d
                        (Fusion.Patch.maybeApply
                            { patch =
                                patch_x__y__z
                                    Fusion.Patch.patcher_Float
                                    Fusion.Patch.patcher_Float
                                    Fusion.Patch.patcher_Float
                            , build =
                                build_x__y__z
                                    Fusion.Patch.patcher_Float
                                    Fusion.Patch.patcher_Float
                                    Fusion.Patch.patcher_Float
                            , toValue =
                                toValue_x__y__z
                                    Fusion.Patch.patcher_Float
                                    Fusion.Patch.patcher_Float
                                    Fusion.Patch.patcher_Float
                            }
                            options
                            patch0
                            arg0
                        )

                ( _, Fusion.Patch.PCustomSame "Point3d" _, _ ) ->
                    Result.Err Fusion.Patch.CouldNotBuildValueFromPatch

                ( _, Fusion.Patch.PCustomSame _ _, _ ) ->
                    Result.Err (Fusion.Patch.WrongType "patchCustom.wrongSame")

                _ ->
                    Result.Err (Fusion.Patch.WrongType "patchCustom.lastBranch")"""
        , """patcher_Point3d :
            Fusion.Patch.Patcher units
            -> Fusion.Patch.Patcher coordinates
            -> Fusion.Patch.Patcher (FakeModule.Point3d units coordinates)
        patcher_Point3d unitsPatcher coordinatesPatcher =
            { patch = patch_Point3d unitsPatcher coordinatesPatcher
            , build = build_Point3d unitsPatcher coordinatesPatcher
            , toValue = toValue_Point3d unitsPatcher coordinatesPatcher
            }"""
        , """
        toValue_Point3d :
            Fusion.Patch.Patcher units
            -> Fusion.Patch.Patcher coordinates
            -> FakeModule.Point3d units coordinates
            -> Fusion.Value
        toValue_Point3d unitsPatcher coordinatesPatcher value =
            case value of
                FakeModule.Point3d arg0 ->
                    Fusion.VCustom
                        "Point3d"
                        [ Fusion.VRecord
                            (Dict.fromList
                                [ ( "x", Fusion.VFloat arg0.x )
                                , ( "y", Fusion.VFloat arg0.y )
                                , ( "z", Fusion.VFloat arg0.z )
                                ]
                            )
                        ]"""
        , """
        type_Point3d : Fusion.Type
        type_Point3d =
            Fusion.TCustom
                "Point3d"
                [ "units", "coordinates" ]
                [ ( "Point3d"
                , [ Fusion.TRecord
                        [ ( "x"
                        , Fusion.TNamed [ "Basics" ] "Float" [] (Just Fusion.TFloat)
                        )
                        , ( "y"
                        , Fusion.TNamed [ "Basics" ] "Float" [] (Just Fusion.TFloat)
                        )
                        , ( "z"
                        , Fusion.TNamed [ "Basics" ] "Float" [] (Just Fusion.TFloat)
                        )
                        ]
                    ]
                )
                ]"""
        ]
